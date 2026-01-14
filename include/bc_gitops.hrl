%%% @doc Type definitions and records for bc_gitops.
%%% @end

-ifndef(BC_GITOPS_HRL).
-define(BC_GITOPS_HRL, true).

%% -----------------------------------------------------------------------------
%% Source Specification - where to get the application from
%% -----------------------------------------------------------------------------

-record(source_spec, {
    type :: release | git | hex | mesh,
    url :: binary() | undefined,
    sha256 :: binary() | undefined,
    ref :: binary() | undefined,  %% For git: branch/tag/commit
    mcid :: binary() | undefined  %% For mesh: Macula Content Identifier
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
%% Icon Specification - visual representation of an application
%% -----------------------------------------------------------------------------

-record(icon_spec, {
    type :: url | base64 | identicon,
    value :: binary() | undefined,  %% URL, base64 data, or undefined for identicon
    mime_type :: binary() | undefined  %% e.g., <<"image/png">>, <<"image/svg+xml">>
}).

%% -----------------------------------------------------------------------------
%% VM Configuration - settings for isolated VM deployment
%% -----------------------------------------------------------------------------

-type isolation_mode() :: embedded | vm.

-record(vm_config, {
    memory_limit :: pos_integer() | undefined,     %% Max heap size in MB (+MMmcs)
    scheduler_limit :: pos_integer() | undefined,  %% Number of schedulers (+S)
    node_prefix :: binary() | undefined,           %% Prefix for node name
    extra_args :: [binary()]                       %% Additional erl/elixir CLI args
}).

%% -----------------------------------------------------------------------------
%% App Specification - defines desired state for an application
%% -----------------------------------------------------------------------------

-record(app_spec, {
    name :: atom(),
    version :: binary(),
    description :: binary() | undefined,  %% Human-readable description
    icon :: #icon_spec{} | undefined,     %% App icon (falls back to identicon)
    source :: #source_spec{},
    env :: #{atom() => term()},
    health :: #health_spec{} | undefined,
    depends_on :: [atom()],
    %% VM isolation settings (v0.6.0+)
    isolation :: isolation_mode(),         %% embedded (default) or vm
    vm_config :: #vm_config{} | undefined  %% Only used when isolation = vm
}).

%% -----------------------------------------------------------------------------
%% App State - current runtime state of an application
%% -----------------------------------------------------------------------------

-record(app_state, {
    name :: atom(),
    version :: binary(),
    description :: binary() | undefined,  %% Human-readable description
    icon :: #icon_spec{} | undefined,     %% App icon
    status :: pending | starting | running | stopped | failed | upgrading,
    path :: file:filename() | undefined,
    pid :: pid() | undefined,
    started_at :: calendar:datetime() | undefined,
    health :: healthy | unhealthy | unknown,
    env :: #{atom() => term()},
    %% VM isolation state (v0.6.0+)
    isolation :: isolation_mode(),        %% embedded or vm
    node :: node() | undefined,           %% Node where app runs (for isolation=vm)
    os_pid :: pos_integer() | undefined   %% OS process ID (for isolation=vm)
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
-define(TELEMETRY_CODE_PURGE, [bc_gitops, code, purge]).

%% Config parsing
-define(TELEMETRY_PARSE_START, [bc_gitops, parse, start]).
-define(TELEMETRY_PARSE_STOP, [bc_gitops, parse, stop]).

%% Health checks
-define(TELEMETRY_HEALTH_CHECK, [bc_gitops, health, check]).

%% VM isolation events (v0.6.0+)
-define(TELEMETRY_VM_SPAWN_START, [bc_gitops, vm, spawn_start]).
-define(TELEMETRY_VM_SPAWN_STOP, [bc_gitops, vm, spawn_stop]).
-define(TELEMETRY_VM_STOP_START, [bc_gitops, vm, stop_start]).
-define(TELEMETRY_VM_STOP_STOP, [bc_gitops, vm, stop_stop]).
-define(TELEMETRY_NODE_UP, [bc_gitops, cluster, node_up]).
-define(TELEMETRY_NODE_DOWN, [bc_gitops, cluster, node_down]).

%% Mesh content events (v0.7.0+)
-define(TELEMETRY_MESH_FETCH_START, [bc_gitops, mesh, fetch_start]).
-define(TELEMETRY_MESH_FETCH_STOP, [bc_gitops, mesh, fetch_stop]).

-endif.
