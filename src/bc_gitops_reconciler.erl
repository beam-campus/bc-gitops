%%% @doc Core reconciliation loop for bc_gitops.
%%%
%%% This gen_server implements the GitOps reconciliation pattern:
%%% 1. Pull latest changes from git
%%% 2. Parse desired state from repository
%%% 3. Compare with current state
%%% 4. Apply necessary changes (deploy, upgrade, remove)
%%%
%%% The reconciler runs on a configurable interval and emits
%%% telemetry events for observability.
%%% @end
-module(bc_gitops_reconciler).

-behaviour(gen_server).

-include("bc_gitops.hrl").

%% API
-export([
    start_link/1,
    reconcile/0,
    status/0,
    get_desired_state/0,
    get_current_state/0,
    deploy/1,
    remove/1,
    upgrade/2
]).

%% gen_server callbacks
-export([
    init/1,
    handle_call/3,
    handle_cast/2,
    handle_info/2,
    terminate/2
]).

%% Exported for testing
-ifdef(TEST).
-export([
    calculate_actions/2,
    sort_by_dependencies/2
]).
-endif.

-define(SERVER, ?MODULE).

%% -----------------------------------------------------------------------------
%% API
%% -----------------------------------------------------------------------------

-spec start_link(map()) -> {ok, pid()} | {error, term()}.
start_link(Config) ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, Config, []).

-spec reconcile() -> ok | {error, term()}.
reconcile() ->
    gen_server:call(?SERVER, reconcile, 30000).

-spec status() -> {ok, map()} | {error, not_running}.
status() ->
    try
        gen_server:call(?SERVER, status)
    catch
        exit:{noproc, _} -> {error, not_running}
    end.

-spec get_desired_state() -> {ok, map()} | {error, term()}.
get_desired_state() ->
    gen_server:call(?SERVER, get_desired_state).

-spec get_current_state() -> {ok, map()} | {error, term()}.
get_current_state() ->
    gen_server:call(?SERVER, get_current_state).

-spec deploy(#app_spec{}) -> ok | {error, term()}.
deploy(AppSpec) ->
    gen_server:call(?SERVER, {deploy, AppSpec}, 60000).

-spec remove(atom()) -> ok | {error, term()}.
remove(AppName) ->
    gen_server:call(?SERVER, {remove, AppName}, 60000).

-spec upgrade(atom(), binary()) -> ok | {error, term()}.
upgrade(AppName, NewVersion) ->
    gen_server:call(?SERVER, {upgrade, AppName, NewVersion}, 60000).

%% -----------------------------------------------------------------------------
%% gen_server callbacks
%% -----------------------------------------------------------------------------

init(Config) ->
    State = #reconciler_state{
        repo_url = to_binary(maps:get(repo_url, Config)),
        local_path = to_binary(maps:get(local_path, Config)),
        branch = to_binary(maps:get(branch, Config)),
        apps_dir = to_binary(maps:get(apps_dir, Config)),
        runtime_module = maps:get(runtime_module, Config),
        reconcile_interval = maps:get(reconcile_interval, Config),
        last_commit = undefined,
        desired_state = #{},
        current_state = #{},
        status = initializing
    },
    %% Start reconciliation loop
    self() ! init_reconcile,
    {ok, State}.

%% @doc Convert a value to binary (for config normalization).
-spec to_binary(binary() | string() | atom()) -> binary().
to_binary(V) when is_binary(V) -> V;
to_binary(V) when is_list(V) -> list_to_binary(V);
to_binary(V) when is_atom(V) -> atom_to_binary(V, utf8).

handle_call(reconcile, _From, State) ->
    case do_reconcile(State) of
        {ok, NewState} ->
            {reply, ok, NewState};
        {error, Reason, NewState} ->
            {reply, {error, Reason}, NewState}
    end;

handle_call(status, _From, State) ->
    #reconciler_state{
        status = Status,
        last_commit = LastCommit,
        current_state = CurrentState
    } = State,
    HealthyCounts = count_healthy(CurrentState),
    Reply = {ok, #{
        status => Status,
        last_commit => LastCommit,
        app_count => maps:size(CurrentState),
        healthy_count => HealthyCounts
    }},
    {reply, Reply, State};

handle_call(get_desired_state, _From, State) ->
    {reply, {ok, State#reconciler_state.desired_state}, State};

handle_call(get_current_state, _From, State) ->
    {reply, {ok, State#reconciler_state.current_state}, State};

handle_call({deploy, AppSpec}, _From, State) ->
    case do_deploy(AppSpec, State) of
        {ok, NewState} ->
            {reply, ok, NewState};
        {error, Reason} ->
            {reply, {error, Reason}, State}
    end;

handle_call({remove, AppName}, _From, State) ->
    case do_remove(AppName, State) of
        {ok, NewState} ->
            {reply, ok, NewState};
        {error, Reason} ->
            {reply, {error, Reason}, State}
    end;

handle_call({upgrade, AppName, NewVersion}, _From, State) ->
    case do_upgrade(AppName, NewVersion, State) of
        {ok, NewState} ->
            {reply, ok, NewState};
        {error, Reason} ->
            {reply, {error, Reason}, State}
    end;

handle_call(_Request, _From, State) ->
    {reply, {error, unknown_request}, State}.

handle_cast(_Request, State) ->
    {noreply, State}.

handle_info(init_reconcile, State) ->
    %% Initial clone/pull
    case bc_gitops_git:ensure_repo(
            State#reconciler_state.repo_url,
            State#reconciler_state.local_path,
            State#reconciler_state.branch) of
        ok ->
            %% Schedule first reconcile
            self() ! reconcile_tick,
            {noreply, State#reconciler_state{status = ready}};
        {error, Reason} ->
            error_logger:error_msg("bc_gitops: Failed to initialize git repo: ~p~n", [Reason]),
            %% Retry after interval
            erlang:send_after(State#reconciler_state.reconcile_interval, self(), init_reconcile),
            {noreply, State#reconciler_state{status = error}}
    end;

handle_info(reconcile_tick, State) ->
    NewState = case do_reconcile(State) of
        {ok, S} -> S;
        {error, _Reason, S} -> S
    end,
    %% Schedule next reconcile
    erlang:send_after(NewState#reconciler_state.reconcile_interval, self(), reconcile_tick),
    {noreply, NewState};

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

%% -----------------------------------------------------------------------------
%% Internal functions - Reconciliation
%% -----------------------------------------------------------------------------

-spec do_reconcile(#reconciler_state{}) -> {ok, #reconciler_state{}} | {error, term(), #reconciler_state{}}.
do_reconcile(State) ->
    emit_telemetry(?TELEMETRY_RECONCILE_START, #{}, #{}),
    StartTime = erlang:monotonic_time(),

    Result = reconcile_steps(State),

    Duration = erlang:monotonic_time() - StartTime,
    case Result of
        {ok, NewState} ->
            emit_telemetry(?TELEMETRY_RECONCILE_STOP, #{duration => Duration}, #{status => success}),
            {ok, NewState};
        {error, Reason, NewState} ->
            emit_telemetry(?TELEMETRY_RECONCILE_ERROR, #{duration => Duration}, #{error => Reason}),
            {error, Reason, NewState}
    end.

-spec reconcile_steps(#reconciler_state{}) -> {ok, #reconciler_state{}} | {error, term(), #reconciler_state{}}.
reconcile_steps(State) ->
    #reconciler_state{
        local_path = LocalPath,
        branch = Branch,
        apps_dir = AppsDir,
        runtime_module = RuntimeMod
    } = State,

    %% Step 1: Pull latest changes
    case bc_gitops_git:pull(LocalPath, Branch) of
        {ok, NewCommit} ->
            %% Step 2: Parse desired state
            RepoAppsDir = filename:join(binary_to_list(LocalPath), binary_to_list(AppsDir)),
            case bc_gitops_parser:parse_apps_dir(RepoAppsDir) of
                {ok, DesiredState} ->
                    %% Step 3: Get current state from runtime
                    CurrentState = get_runtime_state(RuntimeMod, State#reconciler_state.current_state),

                    %% Step 4: Calculate diff
                    Actions = calculate_actions(DesiredState, CurrentState),

                    %% Step 5: Apply actions
                    {NewCurrentState, Errors} = apply_actions(Actions, RuntimeMod, CurrentState),

                    %% Determine status
                    NewStatus = determine_status(Errors, NewCurrentState),

                    NewState = State#reconciler_state{
                        last_commit = NewCommit,
                        desired_state = DesiredState,
                        current_state = NewCurrentState,
                        status = NewStatus
                    },
                    {ok, NewState};
                {error, ParseError} ->
                    {error, {parse_error, ParseError}, State#reconciler_state{status = error}}
            end;
        {error, GitError} ->
            {error, {git_error, GitError}, State#reconciler_state{status = error}}
    end.

-spec get_runtime_state(module(), map()) -> map().
get_runtime_state(RuntimeMod, PreviousState) ->
    case erlang:function_exported(RuntimeMod, get_current_state, 0) of
        true ->
            case RuntimeMod:get_current_state() of
                {ok, State} -> State;
                _ -> PreviousState
            end;
        false ->
            %% Runtime doesn't support state query, use previous
            PreviousState
    end.

-spec calculate_actions(map(), map()) -> [bc_gitops:action()].
calculate_actions(DesiredState, CurrentState) ->
    DesiredApps = maps:keys(DesiredState),
    CurrentApps = maps:keys(CurrentState),

    %% Apps to deploy (in desired but not current)
    ToDeployNames = DesiredApps -- CurrentApps,
    ToDeployActions = [{deploy, maps:get(Name, DesiredState)} || Name <- ToDeployNames],

    %% Apps to remove (in current but not desired)
    ToRemoveNames = CurrentApps -- DesiredApps,
    ToRemoveActions = [{remove, maps:get(Name, CurrentState)} || Name <- ToRemoveNames],

    %% Apps that might need upgrade or reconfigure
    CommonApps = [N || N <- DesiredApps, lists:member(N, CurrentApps)],
    UpdateActions = lists:filtermap(
        fun(Name) ->
            DesiredSpec = maps:get(Name, DesiredState),
            CurrentAppState = maps:get(Name, CurrentState),
            case needs_update(DesiredSpec, CurrentAppState) of
                {upgrade, OldVersion} ->
                    {true, {upgrade, DesiredSpec, OldVersion}};
                reconfigure ->
                    {true, {reconfigure, DesiredSpec}};
                false ->
                    false
            end
        end,
        CommonApps
    ),

    %% Order: removes first, then deploys (with dependency order), then upgrades
    sort_by_dependencies(ToRemoveActions ++ ToDeployActions ++ UpdateActions, DesiredState).

-spec needs_update(#app_spec{}, #app_state{}) -> {upgrade, binary()} | reconfigure | false.
needs_update(#app_spec{version = DesiredVersion, env = DesiredEnv},
             #app_state{version = CurrentVersion, env = CurrentEnv}) ->
    case DesiredVersion =/= CurrentVersion of
        true ->
            {upgrade, CurrentVersion};
        false ->
            case DesiredEnv =/= CurrentEnv of
                true -> reconfigure;
                false -> false
            end
    end.

-spec sort_by_dependencies([bc_gitops:action()], map()) -> [bc_gitops:action()].
sort_by_dependencies(Actions, DesiredState) ->
    %% Separate actions by type
    {RemoveActions, DeployActions, OtherActions} = partition_actions(Actions),

    %% Sort deploy actions: dependencies first (topological order)
    SortedDeploys = topo_sort_deploys(DeployActions, DesiredState),

    %% Sort remove actions: dependents first (reverse topological order)
    SortedRemoves = topo_sort_removes(RemoveActions, DesiredState),

    %% Order: removes first, then deploys, then upgrades/reconfigures
    SortedRemoves ++ SortedDeploys ++ OtherActions.

-spec partition_actions([bc_gitops:action()]) ->
    {[bc_gitops:action()], [bc_gitops:action()], [bc_gitops:action()]}.
partition_actions(Actions) ->
    lists:foldl(
        fun(Action, {Removes, Deploys, Others}) ->
            case Action of
                {remove, _} -> {[Action | Removes], Deploys, Others};
                {deploy, _} -> {Removes, [Action | Deploys], Others};
                _ -> {Removes, Deploys, [Action | Others]}
            end
        end,
        {[], [], []},
        Actions
    ).

%% @doc Topological sort for deploy actions (dependencies first).
%% Uses Kahn's algorithm.
-spec topo_sort_deploys([bc_gitops:action()], map()) -> [bc_gitops:action()].
topo_sort_deploys([], _DesiredState) ->
    [];
topo_sort_deploys(DeployActions, DesiredState) ->
    %% Build adjacency list and in-degree map
    ActionMap = maps:from_list([{get_action_name(A), A} || A <- DeployActions]),
    AppNames = maps:keys(ActionMap),

    %% Calculate in-degrees (number of dependencies within the deploy set)
    InDegrees = lists:foldl(
        fun(Name, Acc) ->
            Deps = get_dependencies(Name, DesiredState),
            %% Only count deps that are in our deploy set
            LocalDeps = [D || D <- Deps, lists:member(D, AppNames)],
            maps:put(Name, length(LocalDeps), Acc)
        end,
        #{},
        AppNames
    ),

    %% Start with nodes that have no dependencies (in-degree = 0)
    Queue = [N || N <- AppNames, maps:get(N, InDegrees, 0) =:= 0],

    %% Process queue
    kahn_sort(Queue, InDegrees, ActionMap, DesiredState, AppNames, []).

-spec kahn_sort([atom()], map(), map(), map(), [atom()], [bc_gitops:action()]) ->
    [bc_gitops:action()].
kahn_sort([], _InDegrees, _ActionMap, _DesiredState, _AppNames, Acc) ->
    lists:reverse(Acc);
kahn_sort([Name | Rest], InDegrees, ActionMap, DesiredState, AppNames, Acc) ->
    Action = maps:get(Name, ActionMap),

    %% Find apps that depend on this one and decrement their in-degree
    Dependents = [N || N <- AppNames, lists:member(Name, get_dependencies(N, DesiredState))],
    {NewInDegrees, NewReady} = lists:foldl(
        fun(Dep, {DegAcc, ReadyAcc}) ->
            NewDeg = maps:get(Dep, DegAcc, 1) - 1,
            NewDegAcc = maps:put(Dep, NewDeg, DegAcc),
            case NewDeg of
                0 -> {NewDegAcc, [Dep | ReadyAcc]};
                _ -> {NewDegAcc, ReadyAcc}
            end
        end,
        {InDegrees, []},
        Dependents
    ),

    kahn_sort(Rest ++ NewReady, NewInDegrees, ActionMap, DesiredState, AppNames, [Action | Acc]).

%% @doc Sort remove actions: dependents should be removed first.
%% This is reverse topological order.
-spec topo_sort_removes([bc_gitops:action()], map()) -> [bc_gitops:action()].
topo_sort_removes([], _DesiredState) ->
    [];
topo_sort_removes(RemoveActions, DesiredState) ->
    %% For removes, we want dependents removed first
    %% So we reverse the dependency relationship
    ActionMap = maps:from_list([{get_action_name(A), A} || A <- RemoveActions]),
    AppNames = maps:keys(ActionMap),

    %% Calculate out-degrees (number of apps that depend on this one within remove set)
    OutDegrees = lists:foldl(
        fun(Name, Acc) ->
            %% Count how many apps in remove set depend on this one
            Dependents = [N || N <- AppNames,
                          N =/= Name,
                          lists:member(Name, get_dependencies(N, DesiredState))],
            maps:put(Name, length(Dependents), Acc)
        end,
        #{},
        AppNames
    ),

    %% Start with nodes that have no dependents (out-degree = 0)
    Queue = [N || N <- AppNames, maps:get(N, OutDegrees, 0) =:= 0],

    %% Process - apps with no dependents can be removed first
    kahn_sort_removes(Queue, OutDegrees, ActionMap, DesiredState, AppNames, []).

-spec kahn_sort_removes([atom()], map(), map(), map(), [atom()], [bc_gitops:action()]) ->
    [bc_gitops:action()].
kahn_sort_removes([], _OutDegrees, _ActionMap, _DesiredState, _AppNames, Acc) ->
    lists:reverse(Acc);
kahn_sort_removes([Name | Rest], OutDegrees, ActionMap, DesiredState, AppNames, Acc) ->
    Action = maps:get(Name, ActionMap),

    %% Find apps this one depends on and decrement their out-degree
    Deps = get_dependencies(Name, DesiredState),
    LocalDeps = [D || D <- Deps, lists:member(D, AppNames)],

    {NewOutDegrees, NewReady} = lists:foldl(
        fun(Dep, {DegAcc, ReadyAcc}) ->
            NewDeg = maps:get(Dep, DegAcc, 1) - 1,
            NewDegAcc = maps:put(Dep, NewDeg, DegAcc),
            case NewDeg of
                0 -> {NewDegAcc, [Dep | ReadyAcc]};
                _ -> {NewDegAcc, ReadyAcc}
            end
        end,
        {OutDegrees, []},
        LocalDeps
    ),

    kahn_sort_removes(Rest ++ NewReady, NewOutDegrees, ActionMap, DesiredState, AppNames, [Action | Acc]).

-spec get_action_name(bc_gitops:action()) -> atom().
get_action_name({deploy, #app_spec{name = Name}}) -> Name;
get_action_name({remove, #app_state{name = Name}}) -> Name;
get_action_name({upgrade, #app_spec{name = Name}, _}) -> Name;
get_action_name({reconfigure, #app_spec{name = Name}}) -> Name.

-spec get_dependencies(atom(), map()) -> [atom()].
get_dependencies(Name, DesiredState) ->
    case maps:find(Name, DesiredState) of
        {ok, #app_spec{depends_on = Deps}} when is_list(Deps) -> Deps;
        _ -> []
    end.

-spec apply_actions([bc_gitops:action()], module(), map()) -> {map(), [term()]}.
apply_actions(Actions, RuntimeMod, CurrentState) ->
    lists:foldl(
        fun(Action, {StateAcc, ErrorsAcc}) ->
            case apply_action(Action, RuntimeMod) of
                {ok, AppState} ->
                    NewState = maps:put(AppState#app_state.name, AppState, StateAcc),
                    {NewState, ErrorsAcc};
                {removed, AppName} ->
                    NewState = maps:remove(AppName, StateAcc),
                    {NewState, ErrorsAcc};
                {error, Reason} ->
                    {StateAcc, [Reason | ErrorsAcc]}
            end
        end,
        {CurrentState, []},
        Actions
    ).

-spec apply_action(bc_gitops:action(), module()) -> {ok, #app_state{}} | {removed, atom()} | {error, term()}.
apply_action({deploy, AppSpec}, RuntimeMod) ->
    emit_telemetry(?TELEMETRY_DEPLOY_START, #{}, #{app => AppSpec#app_spec.name}),
    Result = RuntimeMod:deploy(AppSpec),
    emit_telemetry(?TELEMETRY_DEPLOY_STOP, #{}, #{app => AppSpec#app_spec.name, result => Result}),
    Result;

apply_action({remove, AppState}, RuntimeMod) ->
    AppName = AppState#app_state.name,
    emit_telemetry(?TELEMETRY_REMOVE_START, #{}, #{app => AppName}),
    Result = RuntimeMod:remove(AppName),
    emit_telemetry(?TELEMETRY_REMOVE_STOP, #{}, #{app => AppName, result => Result}),
    case Result of
        ok -> {removed, AppName};
        Error -> Error
    end;

apply_action({upgrade, AppSpec, OldVersion}, RuntimeMod) ->
    emit_telemetry(?TELEMETRY_UPGRADE_START, #{}, #{
        app => AppSpec#app_spec.name,
        from_version => OldVersion,
        to_version => AppSpec#app_spec.version
    }),
    Result = RuntimeMod:upgrade(AppSpec, OldVersion),
    emit_telemetry(?TELEMETRY_UPGRADE_STOP, #{}, #{
        app => AppSpec#app_spec.name,
        result => Result
    }),
    Result;

apply_action({reconfigure, AppSpec}, RuntimeMod) ->
    RuntimeMod:reconfigure(AppSpec).

-spec determine_status([term()], map()) -> atom().
determine_status([], CurrentState) ->
    case all_healthy(CurrentState) of
        true -> synced;
        false -> degraded
    end;
determine_status(_Errors, _CurrentState) ->
    degraded.

-spec all_healthy(map()) -> boolean().
all_healthy(CurrentState) ->
    lists:all(
        fun(AppState) ->
            case AppState of
                #app_state{health = Health, status = Status} ->
                    Health =:= healthy andalso Status =:= running;
                _ ->
                    false
            end
        end,
        maps:values(CurrentState)
    ).

-spec count_healthy(map()) -> non_neg_integer().
count_healthy(CurrentState) ->
    length([1 || V <- maps:values(CurrentState),
                 is_record(V, app_state),
                 V#app_state.health =:= healthy]).

%% -----------------------------------------------------------------------------
%% Internal functions - Manual operations
%% -----------------------------------------------------------------------------

-spec do_deploy(#app_spec{}, #reconciler_state{}) -> {ok, #reconciler_state{}} | {error, term()}.
do_deploy(AppSpec, State) ->
    RuntimeMod = State#reconciler_state.runtime_module,
    case RuntimeMod:deploy(AppSpec) of
        {ok, AppState} ->
            NewCurrentState = maps:put(AppSpec#app_spec.name, AppState, State#reconciler_state.current_state),
            {ok, State#reconciler_state{current_state = NewCurrentState}};
        Error ->
            Error
    end.

-spec do_remove(atom(), #reconciler_state{}) -> {ok, #reconciler_state{}} | {error, term()}.
do_remove(AppName, State) ->
    RuntimeMod = State#reconciler_state.runtime_module,
    case RuntimeMod:remove(AppName) of
        ok ->
            NewCurrentState = maps:remove(AppName, State#reconciler_state.current_state),
            {ok, State#reconciler_state{current_state = NewCurrentState}};
        Error ->
            Error
    end.

-spec do_upgrade(atom(), binary(), #reconciler_state{}) -> {ok, #reconciler_state{}} | {error, term()}.
do_upgrade(AppName, NewVersion, State) ->
    case maps:find(AppName, State#reconciler_state.desired_state) of
        {ok, AppSpec} ->
            NewAppSpec = AppSpec#app_spec{version = NewVersion},
            case maps:find(AppName, State#reconciler_state.current_state) of
                {ok, CurrentAppState} ->
                    RuntimeMod = State#reconciler_state.runtime_module,
                    case RuntimeMod:upgrade(NewAppSpec, CurrentAppState#app_state.version) of
                        {ok, NewAppState} ->
                            NewCurrentState = maps:put(AppName, NewAppState, State#reconciler_state.current_state),
                            {ok, State#reconciler_state{current_state = NewCurrentState}};
                        Error ->
                            Error
                    end;
                error ->
                    {error, app_not_running}
            end;
        error ->
            {error, app_not_found}
    end.

%% -----------------------------------------------------------------------------
%% Telemetry
%% -----------------------------------------------------------------------------

-spec emit_telemetry([atom()], map(), map()) -> ok.
emit_telemetry(EventName, Measurements, Metadata) ->
    telemetry:execute(EventName, Measurements, Metadata).
