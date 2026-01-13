%%% @doc VM spawner for bc_gitops isolated deployment.
%%%
%%% This module handles spawning guest applications as separate BEAM VM
%%% processes and managing their lifecycle.
%%%
%%% == Spawn Strategies ==
%%%
%%% === Mix Projects (Elixir) ===
%%% ```
%%% elixir --sname {node} --cookie {cookie} \
%%%        --erl "+S {schedulers}" \
%%%        -S mix run --no-halt
%%% '''
%%%
%%% === Mix Release ===
%%% ```
%%% RELEASE_NODE={node} RELEASE_COOKIE={cookie} \
%%% ./_build/prod/rel/{app}/bin/{app} start
%%% '''
%%%
%%% === Rebar3 Release (Erlang) ===
%%% ```
%%% ERL_FLAGS="-sname {node} -setcookie {cookie}" \
%%% ./_build/default/rel/{app}/bin/{app} foreground
%%% '''
%%%
%%% @end
-module(bc_gitops_vm_spawner).

-include("bc_gitops.hrl").
-include_lib("kernel/include/logger.hrl").

%% API
-export([
    spawn_vm/4,
    stop_vm/1,
    kill_vm/1,
    is_vm_alive/1,
    get_os_pid/1
]).

%% Internal state record for tracking spawned VMs
-record(vm_handle, {
    node :: node(),
    os_pid :: pos_integer() | undefined,
    port :: port() | undefined,
    app_name :: atom(),
    work_dir :: file:filename()
}).

-type vm_handle() :: #vm_handle{}.
-export_type([vm_handle/0]).

-define(SPAWN_TIMEOUT, 60000).
-define(STOP_TIMEOUT, 10000).

%% -----------------------------------------------------------------------------
%% API
%% -----------------------------------------------------------------------------

%% @doc Spawn a new BEAM VM for an application.
%%
%% Detects the build tool (mix or rebar3) and spawns appropriately.
%% Returns a handle for managing the VM lifecycle.
-spec spawn_vm(atom(), node(), file:filename(), #vm_config{} | undefined) ->
    {ok, vm_handle()} | {error, term()}.
spawn_vm(AppName, NodeName, WorkDir, VmConfig) ->
    Cookie = bc_gitops_cluster:get_cookie(),
    BuildTool = detect_build_tool(WorkDir),

    ?LOG_INFO("[bc_gitops_vm_spawner] Spawning ~p at ~p using ~p",
              [AppName, NodeName, BuildTool]),

    telemetry:execute(?TELEMETRY_VM_SPAWN_START, #{}, #{
        app => AppName,
        node => NodeName,
        build_tool => BuildTool
    }),

    Result = case BuildTool of
        mix ->
            spawn_mix_app(AppName, NodeName, Cookie, WorkDir, VmConfig);
        mix_release ->
            spawn_mix_release(AppName, NodeName, Cookie, WorkDir, VmConfig);
        rebar3 ->
            spawn_rebar3_release(AppName, NodeName, Cookie, WorkDir, VmConfig);
        {error, Reason} ->
            {error, Reason}
    end,

    case Result of
        {ok, Handle} ->
            telemetry:execute(?TELEMETRY_VM_SPAWN_STOP, #{}, #{
                app => AppName,
                node => NodeName,
                status => success,
                os_pid => Handle#vm_handle.os_pid
            }),
            {ok, Handle};
        {error, SpawnError} ->
            telemetry:execute(?TELEMETRY_VM_SPAWN_STOP, #{}, #{
                app => AppName,
                node => NodeName,
                status => error,
                error => SpawnError
            }),
            {error, SpawnError}
    end.

%% @doc Gracefully stop a VM via RPC to :init.stop().
-spec stop_vm(node()) -> ok | {error, term()}.
stop_vm(Node) ->
    ?LOG_INFO("[bc_gitops_vm_spawner] Stopping VM ~p gracefully", [Node]),

    telemetry:execute(?TELEMETRY_VM_STOP_START, #{}, #{node => Node, method => graceful}),

    Result = case rpc:call(Node, init, stop, [], ?STOP_TIMEOUT) of
        ok ->
            %% Wait for node to disconnect
            wait_for_node_down(Node, ?STOP_TIMEOUT);
        {badrpc, nodedown} ->
            %% Already down
            ok;
        {badrpc, Reason} ->
            {error, {rpc_failed, Reason}}
    end,

    telemetry:execute(?TELEMETRY_VM_STOP_STOP, #{}, #{
        node => Node,
        method => graceful,
        status => case Result of ok -> success; _ -> error end
    }),

    Result.

%% @doc Force kill a VM by its OS process ID.
-spec kill_vm(pos_integer()) -> ok | {error, term()}.
kill_vm(OsPid) when is_integer(OsPid) ->
    ?LOG_WARNING("[bc_gitops_vm_spawner] Force killing VM with OS PID ~p", [OsPid]),

    telemetry:execute(?TELEMETRY_VM_STOP_START, #{}, #{os_pid => OsPid, method => kill}),

    %% Try SIGTERM first, then SIGKILL
    _ = os:cmd(io_lib:format("kill -15 ~p 2>/dev/null", [OsPid])),
    timer:sleep(1000),

    %% Check if still alive, use SIGKILL if needed
    case is_os_pid_alive(OsPid) of
        true ->
            _ = os:cmd(io_lib:format("kill -9 ~p 2>/dev/null", [OsPid])),
            timer:sleep(500),
            case is_os_pid_alive(OsPid) of
                true ->
                    telemetry:execute(?TELEMETRY_VM_STOP_STOP, #{}, #{
                        os_pid => OsPid, method => kill, status => error
                    }),
                    {error, kill_failed};
                false ->
                    telemetry:execute(?TELEMETRY_VM_STOP_STOP, #{}, #{
                        os_pid => OsPid, method => kill, status => success
                    }),
                    ok
            end;
        false ->
            telemetry:execute(?TELEMETRY_VM_STOP_STOP, #{}, #{
                os_pid => OsPid, method => kill, status => success
            }),
            ok
    end.

%% @doc Check if a VM handle is still alive.
-spec is_vm_alive(vm_handle()) -> boolean().
is_vm_alive(#vm_handle{os_pid = OsPid}) when is_integer(OsPid) ->
    is_os_pid_alive(OsPid);
is_vm_alive(#vm_handle{node = Node}) ->
    net_adm:ping(Node) =:= pong.

%% @doc Get the OS PID from a VM handle.
-spec get_os_pid(vm_handle()) -> pos_integer() | undefined.
get_os_pid(#vm_handle{os_pid = OsPid}) ->
    OsPid.

%% -----------------------------------------------------------------------------
%% Internal functions - Build Tool Detection
%% -----------------------------------------------------------------------------

-spec detect_build_tool(file:filename()) -> mix | mix_release | rebar3 | {error, term()}.
detect_build_tool(WorkDir) ->
    MixFile = filename:join(WorkDir, "mix.exs"),
    RebarConfig = filename:join(WorkDir, "rebar.config"),

    %% Check for Mix release first
    MixReleasePath = filename:join([WorkDir, "_build", "prod", "rel"]),
    RebarReleasePath = filename:join([WorkDir, "_build", "default", "rel"]),

    cond_check([
        {fun() -> filelib:is_dir(MixReleasePath) end, mix_release},
        {fun() -> filelib:is_dir(RebarReleasePath) end, rebar3},
        {fun() -> filelib:is_file(MixFile) end, mix},
        {fun() -> filelib:is_file(RebarConfig) end, rebar3}
    ]).

-spec cond_check([{fun(() -> boolean()), term()}]) -> term() | {error, no_build_tool}.
cond_check([]) ->
    {error, no_build_tool};
cond_check([{Check, Result} | Rest]) ->
    case Check() of
        true -> Result;
        false -> cond_check(Rest)
    end.

%% -----------------------------------------------------------------------------
%% Internal functions - Mix Spawning
%% -----------------------------------------------------------------------------

-spec spawn_mix_app(atom(), node(), atom(), file:filename(), #vm_config{} | undefined) ->
    {ok, vm_handle()} | {error, term()}.
spawn_mix_app(AppName, NodeName, Cookie, WorkDir, VmConfig) ->
    %% Build command for: elixir --sname node --cookie cookie -S mix run --no-halt
    NodeStr = atom_to_list(NodeName),
    CookieStr = atom_to_list(Cookie),

    ErlArgs = build_erl_args(VmConfig),

    Cmd = io_lib:format(
        "cd ~s && elixir --sname ~s --cookie ~s ~s -S mix run --no-halt",
        [shell_escape(WorkDir), NodeStr, CookieStr, ErlArgs]
    ),

    spawn_with_command(AppName, NodeName, WorkDir, lists:flatten(Cmd)).

-spec spawn_mix_release(atom(), node(), atom(), file:filename(), #vm_config{} | undefined) ->
    {ok, vm_handle()} | {error, term()}.
spawn_mix_release(AppName, NodeName, Cookie, WorkDir, VmConfig) ->
    %% Find the release binary
    RelDir = filename:join([WorkDir, "_build", "prod", "rel"]),
    case find_release_binary(RelDir, AppName) of
        {ok, RelBin} ->
            NodeStr = atom_to_list(NodeName),
            CookieStr = atom_to_list(Cookie),

            ErlArgs = build_erl_flags(VmConfig),

            %% Use daemon mode for releases
            Cmd = io_lib:format(
                "RELEASE_NODE=~s RELEASE_COOKIE=~s ~s ~s daemon",
                [NodeStr, CookieStr, ErlArgs, RelBin]
            ),

            spawn_with_command(AppName, NodeName, WorkDir, lists:flatten(Cmd));
        {error, Reason} ->
            {error, {release_not_found, Reason}}
    end.

-spec spawn_rebar3_release(atom(), node(), atom(), file:filename(), #vm_config{} | undefined) ->
    {ok, vm_handle()} | {error, term()}.
spawn_rebar3_release(AppName, NodeName, Cookie, WorkDir, VmConfig) ->
    %% Find the release binary
    RelDir = filename:join([WorkDir, "_build", "default", "rel"]),
    case find_release_binary(RelDir, AppName) of
        {ok, RelBin} ->
            NodeStr = atom_to_list(NodeName),
            CookieStr = atom_to_list(Cookie),

            ErlFlags = build_erl_flags(VmConfig),

            %% Use foreground mode with nohup for rebar3 releases
            Cmd = io_lib:format(
                "ERL_FLAGS=\"-sname ~s -setcookie ~s ~s\" nohup ~s foreground > /dev/null 2>&1 &",
                [NodeStr, CookieStr, ErlFlags, RelBin]
            ),

            spawn_with_command(AppName, NodeName, WorkDir, lists:flatten(Cmd));
        {error, Reason} ->
            {error, {release_not_found, Reason}}
    end.

%% -----------------------------------------------------------------------------
%% Internal functions - Command Building
%% -----------------------------------------------------------------------------

-spec build_erl_args(#vm_config{} | undefined) -> string().
build_erl_args(undefined) ->
    "";
build_erl_args(#vm_config{memory_limit = Mem, scheduler_limit = Sched, extra_args = Extra}) ->
    MemArg = case Mem of
        undefined -> "";
        M -> io_lib:format("--erl \"+MMmcs ~p\" ", [M * 1024 * 1024])
    end,
    SchedArg = case Sched of
        undefined -> "";
        S -> io_lib:format("--erl \"+S ~p\" ", [S])
    end,
    ExtraArg = case Extra of
        [] -> "";
        _ -> string:join([binary_to_list(A) || A <- Extra], " ") ++ " "
    end,
    lists:flatten([MemArg, SchedArg, ExtraArg]).

-spec build_erl_flags(#vm_config{} | undefined) -> string().
build_erl_flags(undefined) ->
    "";
build_erl_flags(#vm_config{memory_limit = Mem, scheduler_limit = Sched, extra_args = Extra}) ->
    MemFlag = case Mem of
        undefined -> "";
        M -> io_lib:format("+MMmcs ~p ", [M * 1024 * 1024])
    end,
    SchedFlag = case Sched of
        undefined -> "";
        S -> io_lib:format("+S ~p ", [S])
    end,
    ExtraFlags = case Extra of
        [] -> "";
        _ -> string:join([binary_to_list(A) || A <- Extra], " ") ++ " "
    end,
    lists:flatten([MemFlag, SchedFlag, ExtraFlags]).

-spec find_release_binary(file:filename(), atom()) -> {ok, file:filename()} | {error, term()}.
find_release_binary(RelDir, AppName) ->
    AppNameStr = atom_to_list(AppName),
    %% Look for the release binary
    Candidates = [
        filename:join([RelDir, AppNameStr, "bin", AppNameStr]),
        filename:join([RelDir, AppNameStr, "bin", AppNameStr ++ ".sh"])
    ],
    find_executable(Candidates).

-spec find_executable([file:filename()]) -> {ok, file:filename()} | {error, not_found}.
find_executable([]) ->
    {error, not_found};
find_executable([Path | Rest]) ->
    case filelib:is_file(Path) of
        true -> {ok, Path};
        false -> find_executable(Rest)
    end.

-spec shell_escape(string() | binary()) -> string().
shell_escape(S) when is_binary(S) ->
    shell_escape(binary_to_list(S));
shell_escape(S) ->
    "\"" ++ S ++ "\"".

%% -----------------------------------------------------------------------------
%% Internal functions - Process Spawning
%% -----------------------------------------------------------------------------

-spec spawn_with_command(atom(), node(), file:filename(), string()) ->
    {ok, vm_handle()} | {error, term()}.
spawn_with_command(AppName, NodeName, WorkDir, Cmd) ->
    ?LOG_DEBUG("[bc_gitops_vm_spawner] Executing: ~s", [Cmd]),

    %% Open a port to run the command
    Port = open_port({spawn, Cmd}, [
        exit_status,
        stderr_to_stdout,
        {cd, WorkDir}
    ]),

    %% Wait for the node to come up
    case bc_gitops_cluster:wait_for_node(NodeName, ?SPAWN_TIMEOUT) of
        ok ->
            %% Try to get the OS PID of the remote node
            OsPid = get_remote_os_pid(NodeName),
            Handle = #vm_handle{
                node = NodeName,
                os_pid = OsPid,
                port = Port,
                app_name = AppName,
                work_dir = WorkDir
            },
            {ok, Handle};
        {error, timeout} ->
            %% Clean up the port
            catch port_close(Port),
            {error, {spawn_timeout, NodeName}}
    end.

-spec get_remote_os_pid(node()) -> pos_integer() | undefined.
get_remote_os_pid(Node) ->
    case rpc:call(Node, os, getpid, [], 5000) of
        Pid when is_list(Pid) ->
            list_to_integer(Pid);
        _ ->
            undefined
    end.

%% -----------------------------------------------------------------------------
%% Internal functions - Process Monitoring
%% -----------------------------------------------------------------------------

-spec is_os_pid_alive(pos_integer()) -> boolean().
is_os_pid_alive(OsPid) ->
    %% Use kill -0 to check if process exists
    Result = os:cmd(io_lib:format("kill -0 ~p 2>&1", [OsPid])),
    %% If no output, process exists
    Result =:= "".

-spec wait_for_node_down(node(), pos_integer()) -> ok | {error, timeout}.
wait_for_node_down(Node, Timeout) ->
    Deadline = erlang:monotonic_time(millisecond) + Timeout,
    wait_for_node_down_loop(Node, Deadline).

-spec wait_for_node_down_loop(node(), integer()) -> ok | {error, timeout}.
wait_for_node_down_loop(Node, Deadline) ->
    Now = erlang:monotonic_time(millisecond),
    case Now >= Deadline of
        true ->
            {error, timeout};
        false ->
            case net_adm:ping(Node) of
                pang ->
                    ok;
                pong ->
                    timer:sleep(500),
                    wait_for_node_down_loop(Node, Deadline)
            end
    end.
