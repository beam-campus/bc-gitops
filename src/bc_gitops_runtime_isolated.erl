%%% @doc Isolated VM runtime for bc_gitops.
%%%
%%% This runtime module deploys applications in separate BEAM VMs that
%%% auto-cluster with the host. Provides crash isolation, resource limits,
%%% and security boundaries.
%%%
%%% == Usage ==
%%%
%%% This module is used automatically when an app.config specifies:
%%% ```
%%% #{
%%%     name => my_app,
%%%     isolation => vm,
%%%     vm_config => #{memory_limit => 512, scheduler_limit => 2}
%%% }
%%% '''
%%%
%%% == Architecture ==
%%%
%%% Guest apps run in separate OS processes with their own BEAM runtime.
%%% They connect to the host via Erlang distribution, enabling:
%%% - Phoenix.PubSub broadcasts across nodes
%%% - RPC for health checks and lifecycle management
%%% - Process isolation (guest crash doesn't affect host)
%%%
%%% @end
-module(bc_gitops_runtime_isolated).

-behaviour(bc_gitops_runtime).

-include("bc_gitops.hrl").
-include_lib("kernel/include/logger.hrl").

%% bc_gitops_runtime callbacks
-export([
    deploy/1,
    remove/1,
    upgrade/2,
    reconfigure/1,
    get_current_state/0
]).

%% API for managing state
-export([
    store_state/1,
    get_state/1,
    remove_state/1,
    get_all_states/0
]).

-define(STATE_KEY, {?MODULE, app_states}).

%% -----------------------------------------------------------------------------
%% bc_gitops_runtime Callbacks
%% -----------------------------------------------------------------------------

%% @doc Deploy an application in a separate VM.
%%
%% Steps:
%% 1. Fetch and compile the application (via bc_gitops_workspace)
%% 2. Ensure distribution is running
%% 3. Generate node name for the guest
%% 4. Spawn the VM
%% 5. Wait for node to join cluster
%% 6. Verify application is running
-spec deploy(#app_spec{}) -> {ok, #app_state{}} | {error, term()}.
deploy(#app_spec{name = Name, version = Version, source = Source,
                 description = Desc, icon = Icon, env = Env,
                 vm_config = VmConfig} = _AppSpec) ->

    ?LOG_INFO("[bc_gitops_runtime_isolated] Deploying ~p v~s in isolated VM",
              [Name, Version]),

    %% Step 1: Ensure we're running in distributed mode
    case bc_gitops_cluster:ensure_distributed() of
        ok -> ok;
        {error, DistError} ->
            ?LOG_ERROR("[bc_gitops_runtime_isolated] Distribution setup failed: ~p", [DistError]),
            erlang:throw({distribution_failed, DistError})
    end,

    %% Step 2: Fetch and compile the application
    case bc_gitops_workspace:fetch_package(Name, Source) of
        {ok, WorkDir} ->
            %% Step 3: Generate node name
            NodeName = bc_gitops_cluster:generate_node_name(Name, VmConfig),

            %% Step 4: Spawn the VM
            case bc_gitops_vm_spawner:spawn_vm(Name, NodeName, WorkDir, VmConfig) of
                {ok, VmHandle} ->
                    %% Step 5: Verify application is running
                    case bc_gitops_cluster:rpc_check_app(NodeName, Name) of
                        {ok, running} ->
                            OsPid = bc_gitops_vm_spawner:get_os_pid(VmHandle),
                            State = #app_state{
                                name = Name,
                                version = Version,
                                description = Desc,
                                icon = ensure_icon(Icon, Name),
                                status = running,
                                path = WorkDir,
                                pid = undefined,  %% No local PID for isolated
                                started_at = calendar:universal_time(),
                                health = healthy,
                                env = Env,
                                isolation = vm,
                                node = NodeName,
                                os_pid = OsPid
                            },
                            store_state(State),
                            {ok, State};

                        {ok, not_running} ->
                            %% App didn't auto-start, try starting it via RPC
                            case start_app_via_rpc(NodeName, Name) of
                                ok ->
                                    OsPid = bc_gitops_vm_spawner:get_os_pid(VmHandle),
                                    State = #app_state{
                                        name = Name,
                                        version = Version,
                                        description = Desc,
                                        icon = ensure_icon(Icon, Name),
                                        status = running,
                                        path = WorkDir,
                                        pid = undefined,
                                        started_at = calendar:universal_time(),
                                        health = healthy,
                                        env = Env,
                                        isolation = vm,
                                        node = NodeName,
                                        os_pid = OsPid
                                    },
                                    store_state(State),
                                    {ok, State};
                                {error, StartError} ->
                                    %% Clean up the VM
                                    _ = bc_gitops_vm_spawner:stop_vm(NodeName),
                                    {error, {app_start_failed, StartError}}
                            end;

                        {error, RpcError} ->
                            %% Clean up the VM
                            _ = bc_gitops_vm_spawner:stop_vm(NodeName),
                            {error, {rpc_check_failed, RpcError}}
                    end;

                {error, SpawnError} ->
                    {error, {spawn_failed, SpawnError}}
            end;

        {error, FetchError} ->
            {error, {fetch_failed, FetchError}}
    end.

%% @doc Remove an isolated VM application.
%%
%% Gracefully stops the VM and removes state.
-spec remove(atom()) -> ok | {error, term()}.
remove(AppName) ->
    ?LOG_INFO("[bc_gitops_runtime_isolated] Removing ~p", [AppName]),

    case get_state(AppName) of
        {ok, #app_state{node = Node, os_pid = OsPid}} when Node =/= undefined ->
            %% Try graceful shutdown first
            case bc_gitops_vm_spawner:stop_vm(Node) of
                ok ->
                    remove_state(AppName),
                    ok;
                {error, _} when OsPid =/= undefined ->
                    %% Fall back to force kill
                    _ = bc_gitops_vm_spawner:kill_vm(OsPid),
                    remove_state(AppName),
                    ok;
                {error, Reason} ->
                    {error, {stop_failed, Reason}}
            end;
        {ok, #app_state{}} ->
            %% No node info, just remove state
            remove_state(AppName),
            ok;
        {error, not_found} ->
            %% Already removed
            ok
    end.

%% @doc Upgrade an isolated VM application.
%%
%% For isolated VMs, we do a rolling restart:
%% 1. Stop the old VM
%% 2. Deploy the new version
-spec upgrade(#app_spec{}, binary()) -> {ok, #app_state{}} | {error, term()}.
upgrade(#app_spec{name = Name} = AppSpec, OldVersion) ->
    ?LOG_INFO("[bc_gitops_runtime_isolated] Upgrading ~p from ~s to ~s",
              [Name, OldVersion, AppSpec#app_spec.version]),

    %% Stop old version
    case remove(Name) of
        ok ->
            %% Deploy new version
            deploy(AppSpec);
        {error, Reason} ->
            {error, {upgrade_remove_failed, Reason}}
    end.

%% @doc Reconfigure an isolated VM application.
%%
%% Updates the application environment on the remote node via RPC.
-spec reconfigure(#app_spec{}) -> {ok, #app_state{}} | {error, term()}.
reconfigure(#app_spec{name = Name, env = NewEnv} = _AppSpec) ->
    ?LOG_INFO("[bc_gitops_runtime_isolated] Reconfiguring ~p", [Name]),

    case get_state(Name) of
        {ok, #app_state{node = Node} = State} when Node =/= undefined ->
            %% Update env on remote node
            case update_remote_env(Node, Name, NewEnv) of
                ok ->
                    NewState = State#app_state{env = NewEnv},
                    store_state(NewState),
                    {ok, NewState};
                {error, Reason} ->
                    {error, {reconfigure_failed, Reason}}
            end;
        {ok, _State} ->
            {error, {no_node, Name}};
        {error, not_found} ->
            {error, {not_deployed, Name}}
    end.

%% @doc Get current state of all isolated VM applications.
-spec get_current_state() -> {ok, #{atom() => #app_state{}}} | {error, term()}.
get_current_state() ->
    States = get_all_states(),
    %% Update health status for all apps
    UpdatedStates = maps:map(fun(_Name, State) ->
        check_and_update_health(State)
    end, States),
    {ok, UpdatedStates}.

%% -----------------------------------------------------------------------------
%% State Management
%% -----------------------------------------------------------------------------

%% @doc Store application state in persistent_term.
-spec store_state(#app_state{}) -> ok.
store_state(#app_state{name = Name} = State) ->
    States = get_all_states(),
    NewStates = maps:put(Name, State, States),
    persistent_term:put(?STATE_KEY, NewStates),
    ok.

%% @doc Get state for a specific application.
-spec get_state(atom()) -> {ok, #app_state{}} | {error, not_found}.
get_state(Name) ->
    States = get_all_states(),
    case maps:find(Name, States) of
        {ok, State} -> {ok, State};
        error -> {error, not_found}
    end.

%% @doc Remove state for an application.
-spec remove_state(atom()) -> ok.
remove_state(Name) ->
    States = get_all_states(),
    NewStates = maps:remove(Name, States),
    persistent_term:put(?STATE_KEY, NewStates),
    ok.

%% @doc Get all stored states.
-spec get_all_states() -> #{atom() => #app_state{}}.
get_all_states() ->
    try
        persistent_term:get(?STATE_KEY)
    catch
        error:badarg ->
            #{}
    end.

%% -----------------------------------------------------------------------------
%% Internal Functions
%% -----------------------------------------------------------------------------

-spec ensure_icon(#icon_spec{} | undefined, atom()) -> #icon_spec{}.
ensure_icon(undefined, AppName) ->
    DataUri = bc_gitops_identicon:to_data_uri(AppName),
    #icon_spec{
        type = identicon,
        value = DataUri,
        mime_type = <<"image/svg+xml">>
    };
ensure_icon(Icon, _AppName) ->
    Icon.

-spec start_app_via_rpc(node(), atom()) -> ok | {error, term()}.
start_app_via_rpc(Node, AppName) ->
    case rpc:call(Node, application, ensure_all_started, [AppName], 30000) of
        {ok, _Started} ->
            ok;
        {error, Reason} ->
            {error, Reason};
        {badrpc, Reason} ->
            {error, {rpc_failed, Reason}}
    end.

-spec update_remote_env(node(), atom(), map()) -> ok | {error, term()}.
update_remote_env(Node, AppName, Env) ->
    %% Set each env variable on the remote node
    Results = maps:fold(fun(Key, Value, Acc) ->
        case rpc:call(Node, application, set_env, [AppName, Key, Value], 5000) of
            ok -> Acc;
            {badrpc, Reason} -> [{Key, {rpc_failed, Reason}} | Acc]
        end
    end, [], Env),

    case Results of
        [] -> ok;
        Errors -> {error, {partial_update, Errors}}
    end.

-spec check_and_update_health(#app_state{}) -> #app_state{}.
check_and_update_health(#app_state{node = undefined} = State) ->
    State#app_state{status = stopped, health = unhealthy};
check_and_update_health(#app_state{name = Name, node = Node} = State) ->
    case net_adm:ping(Node) of
        pong ->
            %% Node is reachable, check if app is running
            case bc_gitops_cluster:rpc_check_app(Node, Name) of
                {ok, running} ->
                    State#app_state{status = running, health = healthy};
                {ok, not_running} ->
                    State#app_state{status = stopped, health = unhealthy};
                {error, _} ->
                    State#app_state{status = degraded, health = unknown}
            end;
        pang ->
            State#app_state{status = stopped, health = unhealthy}
    end.
