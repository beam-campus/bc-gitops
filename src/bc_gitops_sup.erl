%%% @doc Top-level supervisor for bc_gitops.
%%%
%%% This supervisor manages the reconciler process. The reconciler
%%% is started only if the required configuration is present.
%%% @end
-module(bc_gitops_sup).

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).

%% -----------------------------------------------------------------------------
%% API
%% -----------------------------------------------------------------------------

-spec start_link() -> {ok, pid()} | ignore | {error, term()}.
start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

%% -----------------------------------------------------------------------------
%% Supervisor callbacks
%% -----------------------------------------------------------------------------

-spec init([]) -> {ok, {supervisor:sup_flags(), [supervisor:child_spec()]}}.
init([]) ->
    SupFlags = #{
        strategy => one_for_one,
        intensity => 5,
        period => 60
    },
    ChildSpecs = case get_config() of
        {ok, Config} ->
            [reconciler_spec(Config)];
        {error, _Reason} ->
            %% No configuration - start without reconciler
            %% User can start it manually via bc_gitops:start_reconciler/1
            []
    end,
    {ok, {SupFlags, ChildSpecs}}.

%% -----------------------------------------------------------------------------
%% Internal functions
%% -----------------------------------------------------------------------------

-spec reconciler_spec(map()) -> supervisor:child_spec().
reconciler_spec(Config) ->
    #{
        id => bc_gitops_reconciler,
        start => {bc_gitops_reconciler, start_link, [Config]},
        restart => permanent,
        shutdown => 5000,
        type => worker,
        modules => [bc_gitops_reconciler]
    }.

-spec get_config() -> {ok, map()} | {error, missing_config}.
get_config() ->
    case application:get_env(bc_gitops, repo_url) of
        {ok, RepoUrl} ->
            {ok, #{
                repo_url => to_binary(RepoUrl),
                local_path => get_env_binary(local_path, <<"/var/lib/bc_gitops">>),
                branch => get_env_binary(branch, <<"main">>),
                apps_dir => get_env_binary(apps_dir, <<"apps">>),
                reconcile_interval => application:get_env(bc_gitops, reconcile_interval, 60000),
                runtime_module => application:get_env(bc_gitops, runtime_module, bc_gitops_runtime_default)
            }};
        undefined ->
            {error, missing_config}
    end.

-spec get_env_binary(atom(), binary()) -> binary().
get_env_binary(Key, Default) ->
    case application:get_env(bc_gitops, Key) of
        {ok, Value} -> to_binary(Value);
        undefined -> Default
    end.

-spec to_binary(binary() | string() | atom()) -> binary().
to_binary(V) when is_binary(V) -> V;
to_binary(V) when is_list(V) -> list_to_binary(V);
to_binary(V) when is_atom(V) -> atom_to_binary(V, utf8).
