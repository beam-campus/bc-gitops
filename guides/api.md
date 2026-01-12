# API Reference

This guide provides a quick reference for the bc_gitops public API.

## Core Module: bc_gitops

The `bc_gitops` module is the main facade for interacting with the GitOps reconciler.

### Status & State

```erlang
%% Get overall status
{ok, Status} = bc_gitops:status().
%% #{status => synced | out_of_sync | error,
%%   last_commit => <<"abc123...">>,
%%   app_count => 5,
%%   healthy_count => 5}

%% Get desired state (from Git repository)
{ok, DesiredState} = bc_gitops:get_desired_state().
%% #{app_name => #app_spec{...}, ...}

%% Get current state (running applications)
{ok, CurrentState} = bc_gitops:get_current_state().
%% #{app_name => #app_state{...}, ...}

%% Get specific application status
{ok, AppState} = bc_gitops:get_app_status(my_app).
%% #app_state{name, version, status, started_at, health, env}
```

### Manual Operations

```erlang
%% Trigger immediate reconciliation
ok = bc_gitops:reconcile().
ok = bc_gitops:sync().  % alias for reconcile/0

%% Deploy an application manually (bypasses Git)
AppSpec = #app_spec{name = my_app, version = <<"1.0.0">>, ...},
{ok, AppState} = bc_gitops:deploy(AppSpec).

%% Remove a managed application
ok = bc_gitops:remove(my_app).

%% Upgrade to a specific version
{ok, AppState} = bc_gitops:upgrade(my_app, <<"2.0.0">>).
```

### Reconciler Control

```erlang
%% Start reconciler with custom config
ok = bc_gitops:start_reconciler(#{
    repo_url => <<"https://github.com/myorg/gitops.git">>,
    branch => <<"main">>,
    reconcile_interval => 60000,
    runtime_module => my_app_runtime
}).

%% Stop the reconciler
ok = bc_gitops:stop_reconciler().
```

## Runtime Module API

Implement the `bc_gitops_runtime` behaviour for custom deployment strategies.

### Required Callbacks

```erlang
-callback deploy(AppSpec :: #app_spec{}) ->
    {ok, #app_state{}} | {error, term()}.

-callback remove(AppName :: atom()) ->
    ok | {error, term()}.

-callback upgrade(AppSpec :: #app_spec{}, OldVersion :: binary()) ->
    {ok, #app_state{}} | {error, term()}.

-callback reconfigure(AppSpec :: #app_spec{}) ->
    {ok, #app_state{}} | {error, term()}.

-callback get_current_state() ->
    {ok, #{atom() => #app_state{}}} | {error, term()}.
```

### Default Runtime

The `bc_gitops_runtime_default` module provides a fully functional implementation:

- Fetches packages from hex.pm (rebar3/mix)
- Clones and compiles git repositories
- Clean restarts on version upgrades (v0.3.0+)
- Automatic code path management

## Workspace API (v0.2.0+)

The `bc_gitops_workspace` module handles package fetching and compilation.

```erlang
%% Initialize workspace (creates temp directories)
ok = bc_gitops_workspace:init().

%% Fetch a package from hex.pm
{ok, EbinPath} = bc_gitops_workspace:fetch_package(recon, #source_spec{
    type = hex,
    ref = <<"2.5.3">>
}).

%% Fetch from git
{ok, EbinPath} = bc_gitops_workspace:fetch_package(my_lib, #source_spec{
    type = git,
    url = <<"https://github.com/myorg/my_lib.git">>,
    ref = <<"v1.0.0">>
}).

%% Add ebin directory to code path
ok = bc_gitops_workspace:add_code_path(EbinPath).

%% Clean up workspace
ok = bc_gitops_workspace:cleanup().
```

## Hot Reload API (v0.2.0+)

The `bc_gitops_hot_reload` module provides hot code upgrade utilities.

```erlang
%% Reload a single module
{ok, ModuleName} = bc_gitops_hot_reload:reload_module(my_module).

%% Reload multiple modules
{ok, ReloadedModules} = bc_gitops_hot_reload:reload_modules([mod1, mod2]).

%% Reload only changed modules for an application
{ok, ChangedModules} = bc_gitops_hot_reload:reload_changed_modules(my_app).

%% Check if a module has changed (beam file MD5)
true | false = bc_gitops_hot_reload:module_changed(my_module).

%% Full application upgrade with process suspension
ok = bc_gitops_hot_reload:upgrade_app(my_app, <<"1.0.0">>, <<"2.0.0">>).
```

## Record Definitions

Include the header file to use record definitions:

```erlang
-include_lib("bc_gitops/include/bc_gitops.hrl").
```

### app_spec

```erlang
-record(app_spec, {
    name     :: atom(),           % Application name
    version  :: binary(),         % Version string
    source   :: #source_spec{},   % Where to get the app
    env      :: map(),            % Application environment
    depends_on :: [atom()],       % Dependencies (other apps)
    health   :: #health_spec{} | undefined
}).
```

### source_spec

```erlang
-record(source_spec, {
    type   :: hex | git | release,
    url    :: binary() | undefined,  % For git/release
    ref    :: binary() | undefined,  % Version/branch/tag/commit
    sha256 :: binary() | undefined   % Integrity check for releases
}).
```

### app_state

```erlang
-record(app_state, {
    name       :: atom(),
    version    :: binary(),
    status     :: running | stopped | failed,
    started_at :: calendar:datetime(),
    health     :: healthy | unhealthy | unknown,
    env        :: map()
}).
```

### health_spec

```erlang
-record(health_spec, {
    type     :: http | tcp | custom,
    port     :: pos_integer(),
    path     :: binary() | undefined,  % For http
    interval :: pos_integer(),         % Check interval (ms)
    timeout  :: pos_integer()          % Check timeout (ms)
}).
```

## Telemetry Events

bc_gitops emits telemetry events for observability. Attach handlers to monitor operations:

```erlang
telemetry:attach(
    <<"gitops-logger">>,
    [bc_gitops, reconcile, stop],
    fun(_Event, Measurements, Metadata, _Config) ->
        logger:info("Reconcile: ~p ~p", [Measurements, Metadata])
    end,
    []
).
```

### Available Events

| Event | Measurements | Metadata |
|-------|--------------|----------|
| `[bc_gitops, reconcile, start]` | - | - |
| `[bc_gitops, reconcile, stop]` | `duration` | `status` |
| `[bc_gitops, reconcile, error]` | `duration` | `error` |
| `[bc_gitops, deploy, start]` | - | `app` |
| `[bc_gitops, deploy, stop]` | - | `app`, `result` |
| `[bc_gitops, upgrade, start]` | - | `app`, `from_version`, `to_version` |
| `[bc_gitops, upgrade, stop]` | - | `app`, `result` |
| `[bc_gitops, remove, start]` | - | `app` |
| `[bc_gitops, remove, stop]` | - | `app`, `result` |
| `[bc_gitops, git, pull]` | - | `repo`, `branch` |

## Configuration Options

Configure via `sys.config` or `application:set_env/3`:

| Option | Type | Default | Description |
|--------|------|---------|-------------|
| `repo_url` | string | *required* | Git repository URL |
| `local_path` | string | `/var/lib/bc_gitops` | Local clone path |
| `branch` | string | `"main"` | Git branch to track |
| `apps_dir` | string | `"apps"` | Directory containing app specs |
| `reconcile_interval` | integer | `60000` | Reconcile interval (ms) |
| `runtime_module` | atom | `bc_gitops_runtime_default` | Runtime implementation |

## App Config File Formats

bc_gitops supports three configuration formats for app specs:

| Format | Extension | Requirements |
|--------|-----------|--------------|
| Erlang terms | `.config` | None (built-in) |
| YAML | `.yaml`, `.yml` | `yamerl` dependency |
| JSON | `.json` | OTP 27+ or `jsx`/`jiffy` |

**File search order:** `app.config` → `app.yaml` → `app.yml` → `app.json` → `config.*`

### Optional Dependencies

```erlang
%% For YAML support:
{yamerl, "0.10.0"}

%% For JSON on OTP < 27:
{jsx, "3.1.0"}
%% or
{jiffy, "1.1.1"}
```

Example configuration:

```erlang
[
    {bc_gitops, [
        {repo_url, "https://github.com/myorg/gitops.git"},
        {branch, "main"},
        {reconcile_interval, 30000},
        {runtime_module, bc_gitops_runtime_default}
    ]}
].
```
