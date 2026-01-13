%%% @doc Default runtime implementation for bc_gitops.
%%%
%%% This is a fully functional runtime that can deploy, upgrade, and manage
%%% OTP applications at runtime. It supports both Erlang and Elixir packages
%%% from hex.pm and git sources.
%%%
%%% == Features ==
%%%
%%% - Fetches packages from hex.pm (rebar3 for Erlang, mix for Elixir)
%%% - Clones and compiles git repositories
%%% - Hot code reloading for upgrades
%%% - Proper code path management
%%% - Health monitoring via application status
%%%
%%% == Usage ==
%%%
%%% This module is used automatically by bc_gitops_reconciler when no
%%% custom runtime is specified:
%%% ```
%%% bc_gitops:start_link(#{
%%%     repo_url => <<"https://github.com/myorg/gitops-config">>,
%%%     local_path => <<"/var/lib/bc_gitops/config">>
%%% }).
%%% '''
%%%
%%% == Custom Runtime ==
%%%
%%% For production deployments with specific requirements, implement
%%% the bc_gitops_runtime behaviour with your own logic.
%%%
%%% @end
-module(bc_gitops_runtime_default).

-behaviour(bc_gitops_runtime).

-include("bc_gitops.hrl").

%% bc_gitops_runtime callbacks
-export([
    deploy/1,
    remove/1,
    upgrade/2,
    reconfigure/1,
    get_current_state/0
]).

%% Internal state (stored in persistent_term for simplicity)
-define(STATE_KEY, {?MODULE, app_state}).

%% -----------------------------------------------------------------------------
%% bc_gitops_runtime callbacks
%% -----------------------------------------------------------------------------

%% @doc Deploy an application.
%%
%% This function:
%% 1. Initializes the workspace (if needed)
%% 2. Fetches the package from hex.pm or git
%% 3. Adds compiled code to the VM's code path
%% 4. Sets application environment
%% 5. Starts the application
-spec deploy(#app_spec{}) -> {ok, #app_state{}} | {error, term()}.
deploy(AppSpec) ->
    #app_spec{name = Name, source = Source} = AppSpec,
    %% Ensure workspace is initialized
    ok = bc_gitops_workspace:init(),

    %% Check if already in code path (pre-installed dependency)
    case code:lib_dir(Name) of
        {error, bad_name} ->
            %% Not in code path - need to fetch
            deploy_from_source(AppSpec, Source);
        _LibDir ->
            %% Already available - just start it
            deploy_existing(AppSpec)
    end.

%% @doc Remove (stop and unload) an application.
-spec remove(atom()) -> ok | {error, term()}.
remove(AppName) ->
    %% Stop the application
    case application:stop(AppName) of
        ok -> ok;
        {error, {not_started, AppName}} -> ok;
        {error, Reason} ->
            %% Log but continue with cleanup
            error_logger:warning_msg("Failed to stop ~p: ~p~n", [AppName, Reason])
    end,

    %% Unload the application
    case application:unload(AppName) of
        ok -> ok;
        {error, {not_loaded, AppName}} -> ok;
        {error, _} -> ok
    end,

    %% Remove from workspace (this also removes code paths)
    bc_gitops_workspace:remove_package(AppName),

    %% Remove from our state tracking
    remove_app_state(AppName),
    ok.

%% @doc Upgrade an application by restarting it.
%%
%% Version upgrades always restart the application because:
%% - Application metadata (routes, supervisors, config) may have changed
%% - application:get_key/2 caches values that hot reload doesn't update
%% - Clean restart ensures all initialization code runs
%%
%% Hot code reload is still available for same-version code changes
%% (e.g., tracking a branch) via bc_gitops_hot_reload module.
%%
%% This function:
%% 1. Stops the running application
%% 2. Removes old code paths
%% 3. Fetches the new version
%% 4. Deploys fresh (starts application with new code)
-spec upgrade(#app_spec{}, binary()) -> {ok, #app_state{}} | {error, term()}.
upgrade(AppSpec, _OldVersion) ->
    #app_spec{name = Name} = AppSpec,

    %% Version upgrades always restart for clean initialization
    %% This ensures routes, supervisors, and app metadata are fresh
    ok = remove(Name),
    deploy(AppSpec).

%% @doc Reconfigure an application.
%%
%% Updates the application environment. Some applications may need
%% to be notified of config changes (e.g., via a config_change callback).
-spec reconfigure(#app_spec{}) -> {ok, #app_state{}} | {error, term()}.
reconfigure(AppSpec) ->
    #app_spec{
        name = Name,
        version = Version,
        description = Description,
        icon = Icon,
        env = NewEnv
    } = AppSpec,

    %% Get old env for comparison
    OldEnv = case get_app_state(Name) of
        {ok, #app_state{env = E}} -> E;
        error -> #{}
    end,

    %% Set new environment
    set_app_env(Name, NewEnv),

    %% Notify application of config change (if it supports it)
    notify_config_change(Name, OldEnv, NewEnv),

    %% Update stored state
    case get_app_state(Name) of
        {ok, OldState} ->
            NewState = OldState#app_state{
                description = Description,
                icon = Icon,
                env = NewEnv
            },
            store_app_state(Name, NewState),
            {ok, NewState};
        error ->
            %% App not tracked, create new state
            AppState = #app_state{
                name = Name,
                version = Version,
                description = Description,
                icon = Icon,
                status = running,
                path = code:lib_dir(Name),
                pid = undefined,
                started_at = calendar:universal_time(),
                health = unknown,
                env = NewEnv
            },
            store_app_state(Name, AppState),
            {ok, AppState}
    end.

%% @doc Get the current state of all managed applications.
-spec get_current_state() -> {ok, #{atom() => #app_state{}}}.
get_current_state() ->
    State = get_all_state(),
    %% Update health status for all apps
    UpdatedState = maps:map(
        fun(_Name, AppState) ->
            check_health(AppState)
        end,
        State
    ),
    {ok, UpdatedState}.

%% -----------------------------------------------------------------------------
%% Internal functions - Deployment
%% -----------------------------------------------------------------------------

-spec deploy_from_source(#app_spec{}, #source_spec{}) ->
    {ok, #app_state{}} | {error, term()}.
deploy_from_source(AppSpec, Source) ->
    #app_spec{name = Name} = AppSpec,
    case bc_gitops_workspace:fetch_package(Name, Source) of
        {ok, _PackagePath} ->
            deploy_existing(AppSpec);
        {error, Reason} ->
            {error, {fetch_failed, Reason}}
    end.

-spec deploy_existing(#app_spec{}) -> {ok, #app_state{}} | {error, term()}.
deploy_existing(AppSpec) ->
    #app_spec{
        name = Name,
        version = Version,
        description = Description,
        icon = Icon,
        env = Env
    } = AppSpec,

    %% Set environment before starting
    set_app_env(Name, Env),

    %% Try to start the application
    case application:ensure_all_started(Name) of
        {ok, _Started} ->
            AppState = #app_state{
                name = Name,
                version = Version,
                description = Description,
                icon = Icon,
                status = running,
                path = code:lib_dir(Name),
                pid = undefined,
                started_at = calendar:universal_time(),
                health = unknown,
                env = Env
            },
            store_app_state(Name, AppState),
            {ok, AppState};
        {error, {Name, {already_started, Name}}} ->
            %% Already running
            AppState = #app_state{
                name = Name,
                version = Version,
                description = Description,
                icon = Icon,
                status = running,
                path = code:lib_dir(Name),
                pid = undefined,
                started_at = calendar:universal_time(),
                health = unknown,
                env = Env
            },
            store_app_state(Name, AppState),
            {ok, AppState};
        {error, Reason} ->
            {error, {start_failed, Reason}}
    end.


%% -----------------------------------------------------------------------------
%% Internal functions - State management
%% -----------------------------------------------------------------------------

-spec get_all_state() -> #{atom() => #app_state{}}.
get_all_state() ->
    try
        persistent_term:get(?STATE_KEY)
    catch
        error:badarg -> #{}
    end.

-spec get_app_state(atom()) -> {ok, #app_state{}} | error.
get_app_state(Name) ->
    State = get_all_state(),
    maps:find(Name, State).

-spec store_app_state(atom(), #app_state{}) -> ok.
store_app_state(Name, AppState) ->
    State = get_all_state(),
    NewState = maps:put(Name, AppState, State),
    persistent_term:put(?STATE_KEY, NewState),
    ok.

-spec remove_app_state(atom()) -> ok.
remove_app_state(Name) ->
    State = get_all_state(),
    NewState = maps:remove(Name, State),
    persistent_term:put(?STATE_KEY, NewState),
    ok.

%% -----------------------------------------------------------------------------
%% Internal functions - Environment
%% -----------------------------------------------------------------------------

-spec set_app_env(atom(), map()) -> ok.
set_app_env(AppName, Env) ->
    maps:fold(
        fun(Key, Value, _) ->
            application:set_env(AppName, Key, Value)
        end,
        ok,
        Env
    ).

-spec notify_config_change(atom(), map(), map()) -> ok.
notify_config_change(App, OldEnv, NewEnv) ->
    %% Find changed keys
    AllKeys = lists:usort(maps:keys(OldEnv) ++ maps:keys(NewEnv)),
    Changed = [
        {K, maps:get(K, OldEnv, undefined), maps:get(K, NewEnv, undefined)}
     || K <- AllKeys,
        maps:get(K, OldEnv, undefined) =/= maps:get(K, NewEnv, undefined)
    ],

    case Changed of
        [] ->
            ok;
        _ ->
            %% Try to call the application's config_change callback
            %% This is an optional callback in OTP applications
            Mod = App,  %% Assumes main module has same name as app
            case erlang:function_exported(Mod, config_change, 3) of
                true ->
                    Removed = [K || {K, _, undefined} <- Changed],
                    New = [{K, V} || {K, undefined, V} <- Changed, V =/= undefined],
                    ChangedKV = [{K, V} || {K, Old, V} <- Changed,
                                           Old =/= undefined, V =/= undefined],
                    catch Mod:config_change(ChangedKV, New, Removed),
                    ok;
                false ->
                    ok
            end
    end.

%% -----------------------------------------------------------------------------
%% Internal functions - Health
%% -----------------------------------------------------------------------------

-spec check_health(#app_state{}) -> #app_state{}.
check_health(AppState = #app_state{name = Name}) ->
    %% Check if the application is running
    case lists:keyfind(Name, 1, application:which_applications()) of
        {Name, _, _} ->
            AppState#app_state{status = running, health = healthy};
        false ->
            AppState#app_state{status = stopped, health = unhealthy}
    end.
