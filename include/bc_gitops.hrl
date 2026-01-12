%%% @doc Type definitions and records for bc_gitops.
%%% @end

-ifndef(BC_GITOPS_HRL).
-define(BC_GITOPS_HRL, true).

%% -----------------------------------------------------------------------------
%% Source Specification - where to get the application from
%% -----------------------------------------------------------------------------

-record(source_spec, {
    type :: release | git | hex,
    url :: binary() | undefined,
    sha256 :: binary() | undefined,
    ref :: binary() | undefined  %% For git: branch/tag/commit
}).

%% -----------------------------------------------------------------------------
%% Health Specification - how to check application health
%% -----------------------------------------------------------------------------

-record(health_spec, {
    type :: http | tcp | custom,
    port :: pos_integer(),
    path :: binary() | undefined,  %% For HTTP
    interval :: pos_integer(),     %% Milliseconds
    timeout :: pos_integer(),      %% Milliseconds
    module :: module() | undefined %% For custom health checks
}).

%% -----------------------------------------------------------------------------
%% App Specification - defines desired state for an application
%% -----------------------------------------------------------------------------

-record(app_spec, {
    name :: atom(),
    version :: binary(),
    source :: #source_spec{},
    env :: #{atom() => term()},
    health :: #health_spec{} | undefined,
    depends_on :: [atom()]
}).

%% -----------------------------------------------------------------------------
%% App State - current runtime state of an application
%% -----------------------------------------------------------------------------

-record(app_state, {
    name :: atom(),
    version :: binary(),
    status :: pending | starting | running | stopped | failed | upgrading,
    path :: file:filename() | undefined,
    pid :: pid() | undefined,
    started_at :: calendar:datetime() | undefined,
    health :: healthy | unhealthy | unknown,
    env :: #{atom() => term()}
}).

%% -----------------------------------------------------------------------------
%% Reconciler State
%% -----------------------------------------------------------------------------

-record(reconciler_state, {
    repo_url :: binary(),
    local_path :: binary(),
    branch :: binary(),
    apps_dir :: binary(),
    runtime_module :: module(),
    reconcile_interval :: pos_integer(),
    last_commit :: binary() | undefined,
    desired_state :: #{atom() => #app_spec{}},
    current_state :: #{atom() => #app_state{}},
    status :: initializing | ready | synced | degraded | error
}).

%% -----------------------------------------------------------------------------
%% Actions - operations to perform during reconciliation
%% -----------------------------------------------------------------------------

-type action() ::
    {deploy, #app_spec{}} |
    {remove, #app_state{}} |
    {upgrade, #app_spec{}, OldVersion :: binary()} |
    {reconfigure, #app_spec{}}.

%% -----------------------------------------------------------------------------
%% Telemetry Event Names
%% -----------------------------------------------------------------------------

%% Reconciliation events
-define(TELEMETRY_RECONCILE_START, [bc_gitops, reconcile, start]).
-define(TELEMETRY_RECONCILE_STOP, [bc_gitops, reconcile, stop]).
-define(TELEMETRY_RECONCILE_ERROR, [bc_gitops, reconcile, error]).

%% Deploy/upgrade/remove events
-define(TELEMETRY_DEPLOY_START, [bc_gitops, deploy, start]).
-define(TELEMETRY_DEPLOY_STOP, [bc_gitops, deploy, stop]).
-define(TELEMETRY_UPGRADE_START, [bc_gitops, upgrade, start]).
-define(TELEMETRY_UPGRADE_STOP, [bc_gitops, upgrade, stop]).
-define(TELEMETRY_REMOVE_START, [bc_gitops, remove, start]).
-define(TELEMETRY_REMOVE_STOP, [bc_gitops, remove, stop]).

%% Git operations
-define(TELEMETRY_GIT_PULL, [bc_gitops, git, pull]).
-define(TELEMETRY_GIT_CLONE_START, [bc_gitops, git, clone_start]).
-define(TELEMETRY_GIT_CLONE_STOP, [bc_gitops, git, clone_stop]).

%% Build operations
-define(TELEMETRY_BUILD_START, [bc_gitops, build, start]).
-define(TELEMETRY_BUILD_STOP, [bc_gitops, build, stop]).
-define(TELEMETRY_DEPS_START, [bc_gitops, deps, start]).
-define(TELEMETRY_DEPS_STOP, [bc_gitops, deps, stop]).

%% Code loading
-define(TELEMETRY_CODE_LOAD, [bc_gitops, code, load]).

%% Config parsing
-define(TELEMETRY_PARSE_START, [bc_gitops, parse, start]).
-define(TELEMETRY_PARSE_STOP, [bc_gitops, parse, stop]).

%% Health checks
-define(TELEMETRY_HEALTH_CHECK, [bc_gitops, health, check]).

-endif.
