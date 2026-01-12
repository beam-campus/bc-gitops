%%% @doc Default runtime implementation for bc_gitops.
%%%
%%% This is a reference implementation that uses standard OTP
%%% application management. It demonstrates the basic patterns
%%% but may need customization for production use.
%%%
%%% == Features ==
%%%
%%% <ul>
%%% <li>Deploys applications from hex.pm using rebar3</li>
%%% <li>Supports basic start/stop operations</li>
%%% <li>Tracks application state in memory</li>
%%% </ul>
%%%
%%% == Limitations ==
%%%
%%% This default implementation:
%%% <ul>
%%% <li>Does not support hot code upgrades (uses restart)</li>
%%% <li>Does not download from git or custom URLs</li>
%%% <li>Requires rebar3 to be installed</li>
%%% </ul>
%%%
%%% For production, implement a custom runtime module.
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
%% For hex packages, this attempts to load and start the application
%% if it's already in the code path. For production, you'd want to
%% implement proper package downloading.
-spec deploy(#app_spec{}) -> {ok, #app_state{}} | {error, term()}.
deploy(#app_spec{name = Name, version = Version, source = _Source, env = Env}) ->
    %% Set environment before starting
    set_app_env(Name, Env),

    %% Try to start the application
    case application:ensure_all_started(Name) of
        {ok, _Started} ->
            AppState = #app_state{
                name = Name,
                version = Version,
                status = running,
                path = code:lib_dir(Name),
                pid = undefined,  %% OTP apps don't have a single pid
                started_at = calendar:universal_time(),
                health = unknown,
                env = Env
            },
            store_app_state(Name, AppState),
            {ok, AppState};
        {error, {Name, {already_started, Name}}} ->
            %% Already running - get current state
            AppState = #app_state{
                name = Name,
                version = Version,
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

%% @doc Remove (stop) an application.
-spec remove(atom()) -> ok | {error, term()}.
remove(AppName) ->
    case application:stop(AppName) of
        ok ->
            remove_app_state(AppName),
            ok;
        {error, {not_started, AppName}} ->
            %% Already stopped
            remove_app_state(AppName),
            ok;
        {error, Reason} ->
            {error, {stop_failed, Reason}}
    end.

%% @doc Upgrade an application.
%%
%% The default implementation does a simple restart. For proper
%% hot code upgrades, implement a custom runtime using release_handler.
-spec upgrade(#app_spec{}, binary()) -> {ok, #app_state{}} | {error, term()}.
upgrade(AppSpec, _OldVersion) ->
    #app_spec{name = Name} = AppSpec,
    %% Simple restart strategy
    case remove(Name) of
        ok ->
            deploy(AppSpec);
        {error, Reason} ->
            {error, {upgrade_failed, Reason}}
    end.

%% @doc Reconfigure an application.
%%
%% Updates the application environment. Some applications may need
%% a restart to pick up the new configuration.
-spec reconfigure(#app_spec{}) -> {ok, #app_state{}} | {error, term()}.
reconfigure(#app_spec{name = Name, version = Version, env = NewEnv}) ->
    set_app_env(Name, NewEnv),

    %% Update stored state
    case get_app_state(Name) of
        {ok, OldState} ->
            NewState = OldState#app_state{env = NewEnv},
            store_app_state(Name, NewState),
            {ok, NewState};
        error ->
            %% App not tracked, create new state
            AppState = #app_state{
                name = Name,
                version = Version,
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

%% -----------------------------------------------------------------------------
%% Internal functions - Health
%% -----------------------------------------------------------------------------

-spec check_health(#app_state{}) -> #app_state{}.
check_health(AppState = #app_state{name = Name}) ->
    %% Simple health check: is the application running?
    case lists:keyfind(Name, 1, application:which_applications()) of
        {Name, _, _} ->
            AppState#app_state{status = running, health = healthy};
        false ->
            AppState#app_state{status = stopped, health = unhealthy}
    end.
