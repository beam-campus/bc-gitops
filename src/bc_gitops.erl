%%% @doc Main API module for bc_gitops.
%%%
%%% bc_gitops is a BEAM-native GitOps reconciler for OTP applications.
%%% It monitors a Git repository and automatically deploys, upgrades,
%%% or removes applications based on the desired state defined in the repo.
%%%
%%% == Quick Start ==
%%%
%%% 1. Configure the application in your sys.config:
%%% ```
%%% {bc_gitops, [
%%%     {repo_url, "https://github.com/myorg/gitops-repo.git"},
%%%     {runtime_module, my_app_runtime}
%%% ]}
%%% ```
%%%
%%% 2. Implement the `bc_gitops_runtime' behaviour in your runtime module.
%%%
%%% 3. Start the application:
%%% ```
%%% application:start(bc_gitops).
%%% '''
%%%
%%% == Configuration ==
%%%
%%% <ul>
%%% <li>`repo_url' - Git repository URL (required)</li>
%%% <li>`local_path' - Local clone path (default: /var/lib/bc_gitops)</li>
%%% <li>`branch' - Git branch to track (default: main)</li>
%%% <li>`apps_dir' - Directory within repo containing app specs (default: apps)</li>
%%% <li>`reconcile_interval' - Interval between reconciliations in ms (default: 60000)</li>
%%% <li>`runtime_module' - Module implementing bc_gitops_runtime behaviour</li>
%%% </ul>
%%%
%%% @end
-module(bc_gitops).

-include("bc_gitops.hrl").

%% API
-export([
    %% Lifecycle
    start_reconciler/1,
    stop_reconciler/0,

    %% Manual operations
    reconcile/0,
    sync/0,

    %% Status
    status/0,
    get_desired_state/0,
    get_current_state/0,
    get_app_status/1,

    %% Manual app control
    deploy/1,
    remove/1,
    upgrade/2
]).

%% Types
-export_type([
    app_spec/0,
    app_state/0,
    source_spec/0,
    health_spec/0,
    action/0
]).

-type app_spec() :: #app_spec{}.
-type app_state() :: #app_state{}.
-type source_spec() :: #source_spec{}.
-type health_spec() :: #health_spec{}.
%% action() type is defined in bc_gitops.hrl

%% -----------------------------------------------------------------------------
%% Lifecycle API
%% -----------------------------------------------------------------------------

%% @doc Start the reconciler with the given configuration.
%%
%% This is useful when you want to start the reconciler programmatically
%% instead of through application configuration.
%%
%% == Example ==
%% ```
%% bc_gitops:start_reconciler(#{
%%     repo_url => <<"https://github.com/myorg/gitops.git">>,
%%     runtime_module => my_runtime
%% }).
%% '''
-spec start_reconciler(map()) -> {ok, pid()} | {error, term()}.
start_reconciler(Config) ->
    FullConfig = maps:merge(default_config(), Config),
    ChildSpec = #{
        id => bc_gitops_reconciler,
        start => {bc_gitops_reconciler, start_link, [FullConfig]},
        restart => permanent,
        shutdown => 5000,
        type => worker,
        modules => [bc_gitops_reconciler]
    },
    supervisor:start_child(bc_gitops_sup, ChildSpec).

%% @doc Stop the running reconciler.
-spec stop_reconciler() -> ok | {error, term()}.
stop_reconciler() ->
    case supervisor:terminate_child(bc_gitops_sup, bc_gitops_reconciler) of
        ok -> supervisor:delete_child(bc_gitops_sup, bc_gitops_reconciler);
        Error -> Error
    end.

%% -----------------------------------------------------------------------------
%% Manual Operations
%% -----------------------------------------------------------------------------

%% @doc Trigger an immediate reconciliation.
%%
%% This pulls the latest changes from git and reconciles the
%% current state with the desired state.
-spec reconcile() -> ok | {error, term()}.
reconcile() ->
    bc_gitops_reconciler:reconcile().

%% @doc Alias for reconcile/0.
-spec sync() -> ok | {error, term()}.
sync() ->
    reconcile().

%% -----------------------------------------------------------------------------
%% Status API
%% -----------------------------------------------------------------------------

%% @doc Get the current reconciler status.
%%
%% Returns a map containing:
%% <ul>
%% <li>`status' - Current reconciler status (initializing | ready | synced | degraded | error)</li>
%% <li>`last_commit' - SHA of the last processed commit</li>
%% <li>`app_count' - Number of managed applications</li>
%% <li>`healthy_count' - Number of healthy applications</li>
%% </ul>
-spec status() -> {ok, map()} | {error, not_running}.
status() ->
    bc_gitops_reconciler:status().

%% @doc Get the desired state (from git repository).
-spec get_desired_state() -> {ok, #{atom() => app_spec()}} | {error, term()}.
get_desired_state() ->
    bc_gitops_reconciler:get_desired_state().

%% @doc Get the current running state.
-spec get_current_state() -> {ok, #{atom() => app_state()}} | {error, term()}.
get_current_state() ->
    bc_gitops_reconciler:get_current_state().

%% @doc Get the status of a specific application.
-spec get_app_status(atom()) -> {ok, app_state()} | {error, not_found}.
get_app_status(AppName) ->
    case get_current_state() of
        {ok, State} ->
            case maps:find(AppName, State) of
                {ok, AppState} -> {ok, AppState};
                error -> {error, not_found}
            end;
        Error ->
            Error
    end.

%% -----------------------------------------------------------------------------
%% Manual App Control
%% -----------------------------------------------------------------------------

%% @doc Manually deploy an application.
%%
%% This bypasses the git repository and deploys an application
%% directly from the provided spec. The deployment will be
%% overwritten on the next reconciliation if the app is not
%% defined in the git repository.
-spec deploy(app_spec()) -> ok | {error, term()}.
deploy(AppSpec) ->
    bc_gitops_reconciler:deploy(AppSpec).

%% @doc Manually remove an application.
%%
%% This removes an application regardless of the desired state.
%% The application will be redeployed on the next reconciliation
%% if it's defined in the git repository.
-spec remove(atom()) -> ok | {error, term()}.
remove(AppName) ->
    bc_gitops_reconciler:remove(AppName).

%% @doc Manually upgrade an application to a new version.
-spec upgrade(atom(), binary()) -> ok | {error, term()}.
upgrade(AppName, NewVersion) ->
    bc_gitops_reconciler:upgrade(AppName, NewVersion).

%% -----------------------------------------------------------------------------
%% Internal functions
%% -----------------------------------------------------------------------------

-spec default_config() -> map().
default_config() ->
    #{
        local_path => <<"/var/lib/bc_gitops">>,
        branch => <<"main">>,
        apps_dir => <<"apps">>,
        reconcile_interval => 60000,
        runtime_module => bc_gitops_runtime_default
    }.
