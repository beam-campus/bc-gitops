%%% @doc Cluster management for bc_gitops isolated VM deployment.
%%%
%%% This module handles Erlang distribution setup and node management
%%% for running guest applications in separate BEAM VMs.
%%%
%%% == Cookie Management ==
%%%
%%% Erlang cookies are resolved in this order:
%%% 1. Application env: `{bc_gitops, [{cookie, <<"secret">>}]}'
%%% 2. Environment variable: `RELEASE_COOKIE' or `ERLANG_COOKIE'
%%% 3. User's ~/.erlang.cookie file
%%% 4. Auto-generated (persisted to ~/.erlang.cookie)
%%%
%%% == Node Naming ==
%%%
%%% Guest nodes are named using the pattern:
%%% `{prefix}{app_name}@{hostname}'
%%%
%%% Default prefix is empty, configurable via vm_config.node_prefix.
%%%
%%% @end
-module(bc_gitops_cluster).

-include("bc_gitops.hrl").
-include_lib("kernel/include/logger.hrl").

%% API
-export([
    ensure_distributed/0,
    get_cookie/0,
    set_cookie/1,
    generate_node_name/2,
    wait_for_node/2,
    is_node_connected/1,
    rpc_check_app/2,
    get_hostname/0,
    monitor_nodes/0,
    unmonitor_nodes/0
]).

%% Internal exports for testing
-export([
    resolve_cookie/0,
    read_cookie_file/0
]).

-define(DEFAULT_NODE_WAIT_TIMEOUT, 30000).
-define(NODE_POLL_INTERVAL, 500).
-define(RPC_TIMEOUT, 5000).

%% -----------------------------------------------------------------------------
%% API
%% -----------------------------------------------------------------------------

%% @doc Ensure this node is running in distributed mode.
%% Starts distribution if not already running.
-spec ensure_distributed() -> ok | {error, term()}.
ensure_distributed() ->
    case node() of
        nonode@nohost ->
            start_distribution();
        _ ->
            %% Already distributed
            ok
    end.

%% @doc Get the Erlang cookie for the cluster.
%% Resolves from config, env vars, or file. Generates if not found.
-spec get_cookie() -> atom().
get_cookie() ->
    case resolve_cookie() of
        {ok, Cookie} -> Cookie;
        {error, not_found} ->
            %% Generate and persist a new cookie
            NewCookie = generate_cookie(),
            ok = persist_cookie(NewCookie),
            NewCookie
    end.

%% @doc Set the Erlang cookie for this node and persist it.
-spec set_cookie(atom() | binary()) -> ok.
set_cookie(Cookie) when is_binary(Cookie) ->
    set_cookie(binary_to_atom(Cookie, utf8));
set_cookie(Cookie) when is_atom(Cookie) ->
    true = erlang:set_cookie(node(), Cookie),
    ok = persist_cookie(Cookie),
    ok.

%% @doc Generate a node name for a guest application.
%%
%% Format: {prefix}{app_name}@{hostname}
%%
%% If vm_config has a node_prefix, it's prepended to the app name.
-spec generate_node_name(atom(), #vm_config{} | undefined) -> node().
generate_node_name(AppName, undefined) ->
    generate_node_name(AppName, #vm_config{node_prefix = undefined});
generate_node_name(AppName, #vm_config{node_prefix = undefined}) ->
    Hostname = get_hostname(),
    list_to_atom(atom_to_list(AppName) ++ "@" ++ Hostname);
generate_node_name(AppName, #vm_config{node_prefix = Prefix}) ->
    Hostname = get_hostname(),
    PrefixStr = binary_to_list(Prefix),
    list_to_atom(PrefixStr ++ atom_to_list(AppName) ++ "@" ++ Hostname).

%% @doc Wait for a node to appear in the cluster.
%% Returns ok when node is connected, or {error, timeout} after timeout.
-spec wait_for_node(node(), pos_integer()) -> ok | {error, timeout}.
wait_for_node(Node, Timeout) ->
    Deadline = erlang:monotonic_time(millisecond) + Timeout,
    wait_for_node_loop(Node, Deadline).

%% @doc Check if a node is currently connected.
-spec is_node_connected(node()) -> boolean().
is_node_connected(Node) ->
    lists:member(Node, nodes()).

%% @doc Check if an application is running on a remote node via RPC.
-spec rpc_check_app(node(), atom()) -> {ok, running} | {ok, not_running} | {error, term()}.
rpc_check_app(Node, AppName) ->
    case rpc:call(Node, application, which_applications, [], ?RPC_TIMEOUT) of
        {badrpc, Reason} ->
            {error, {rpc_failed, Reason}};
        Apps when is_list(Apps) ->
            case lists:keyfind(AppName, 1, Apps) of
                {AppName, _, _} -> {ok, running};
                false -> {ok, not_running}
            end
    end.

%% @doc Get the short hostname of this machine.
-spec get_hostname() -> string().
get_hostname() ->
    {ok, Hostname} = inet:gethostname(),
    Hostname.

%% @doc Subscribe to node up/down events.
-spec monitor_nodes() -> ok.
monitor_nodes() ->
    ok = net_kernel:monitor_nodes(true),
    ok.

%% @doc Unsubscribe from node up/down events.
-spec unmonitor_nodes() -> ok.
unmonitor_nodes() ->
    ok = net_kernel:monitor_nodes(false),
    ok.

%% -----------------------------------------------------------------------------
%% Internal functions - Distribution
%% -----------------------------------------------------------------------------

-spec start_distribution() -> ok | {error, term()}.
start_distribution() ->
    %% Generate a unique node name for the host
    Hostname = get_hostname(),
    NodeName = list_to_atom("bc_gitops_host@" ++ Hostname),

    case net_kernel:start([NodeName, shortnames]) of
        {ok, _Pid} ->
            ?LOG_INFO("[bc_gitops_cluster] Started distribution as ~p", [NodeName]),
            %% Set the cookie
            Cookie = get_cookie(),
            true = erlang:set_cookie(node(), Cookie),
            ok;
        {error, {already_started, _Pid}} ->
            ok;
        {error, Reason} ->
            ?LOG_ERROR("[bc_gitops_cluster] Failed to start distribution: ~p", [Reason]),
            {error, {distribution_failed, Reason}}
    end.

%% -----------------------------------------------------------------------------
%% Internal functions - Cookie Management
%% -----------------------------------------------------------------------------

%% @doc Resolve the cookie from various sources.
-spec resolve_cookie() -> {ok, atom()} | {error, not_found}.
resolve_cookie() ->
    %% Try sources in order of priority
    Sources = [
        fun() -> get_cookie_from_app_env() end,
        fun() -> get_cookie_from_env_var() end,
        fun() -> read_cookie_file() end
    ],
    try_sources(Sources).

-spec try_sources([fun(() -> {ok, atom()} | {error, term()})]) -> {ok, atom()} | {error, not_found}.
try_sources([]) ->
    {error, not_found};
try_sources([Source | Rest]) ->
    case Source() of
        {ok, Cookie} -> {ok, Cookie};
        {error, _} -> try_sources(Rest)
    end.

-spec get_cookie_from_app_env() -> {ok, atom()} | {error, not_found}.
get_cookie_from_app_env() ->
    case application:get_env(bc_gitops, cookie) of
        {ok, Cookie} when is_atom(Cookie) -> {ok, Cookie};
        {ok, Cookie} when is_binary(Cookie) -> {ok, binary_to_atom(Cookie, utf8)};
        {ok, Cookie} when is_list(Cookie) -> {ok, list_to_atom(Cookie)};
        _ -> {error, not_found}
    end.

-spec get_cookie_from_env_var() -> {ok, atom()} | {error, not_found}.
get_cookie_from_env_var() ->
    EnvVars = ["RELEASE_COOKIE", "ERLANG_COOKIE", "BC_GITOPS_COOKIE"],
    get_first_env_var(EnvVars).

-spec get_first_env_var([string()]) -> {ok, atom()} | {error, not_found}.
get_first_env_var([]) ->
    {error, not_found};
get_first_env_var([Var | Rest]) ->
    case os:getenv(Var) of
        false -> get_first_env_var(Rest);
        "" -> get_first_env_var(Rest);
        Value -> {ok, list_to_atom(Value)}
    end.

-spec read_cookie_file() -> {ok, atom()} | {error, term()}.
read_cookie_file() ->
    CookieFile = cookie_file_path(),
    case file:read_file(CookieFile) of
        {ok, Content} ->
            %% Cookie file contains the cookie as a single line
            Cookie = string:trim(binary_to_list(Content)),
            case Cookie of
                "" -> {error, empty_cookie};
                _ -> {ok, list_to_atom(Cookie)}
            end;
        {error, Reason} ->
            {error, {cookie_file_read_failed, Reason}}
    end.

-spec cookie_file_path() -> file:filename().
cookie_file_path() ->
    case os:getenv("HOME") of
        false -> "/tmp/.erlang.cookie";
        Home -> filename:join(Home, ".erlang.cookie")
    end.

-spec generate_cookie() -> atom().
generate_cookie() ->
    %% Generate a random 20-character cookie
    Bytes = crypto:strong_rand_bytes(15),
    Hex = binary:encode_hex(Bytes),
    binary_to_atom(Hex, utf8).

-spec persist_cookie(atom()) -> ok | {error, term()}.
persist_cookie(Cookie) ->
    CookieFile = cookie_file_path(),
    CookieStr = atom_to_list(Cookie) ++ "\n",
    case file:write_file(CookieFile, CookieStr) of
        ok ->
            %% Set restrictive permissions (owner read/write only)
            _ = file:change_mode(CookieFile, 8#600),
            ok;
        {error, Reason} ->
            ?LOG_WARNING("[bc_gitops_cluster] Failed to persist cookie: ~p", [Reason]),
            {error, {cookie_persist_failed, Reason}}
    end.

%% -----------------------------------------------------------------------------
%% Internal functions - Node Waiting
%% -----------------------------------------------------------------------------

-spec wait_for_node_loop(node(), integer()) -> ok | {error, timeout}.
wait_for_node_loop(Node, Deadline) ->
    Now = erlang:monotonic_time(millisecond),
    case Now >= Deadline of
        true ->
            {error, timeout};
        false ->
            case net_adm:ping(Node) of
                pong ->
                    ok;
                pang ->
                    timer:sleep(?NODE_POLL_INTERVAL),
                    wait_for_node_loop(Node, Deadline)
            end
    end.
