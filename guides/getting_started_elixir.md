# Getting Started with bc_gitops (Elixir)

This guide walks you through setting up bc_gitops to manage OTP applications from an Elixir project.

## What is GitOps?

GitOps is an operational framework where:

1. **Git is the source of truth** - The desired state of your system is stored in a Git repository
2. **Declarative configuration** - You describe *what* you want, not *how* to achieve it
3. **Automatic reconciliation** - The system continuously compares desired vs actual state and takes corrective actions

bc_gitops brings this pattern to the BEAM ecosystem, allowing you to manage OTP applications the same way Flux or ArgoCD manage Kubernetes workloads.

## Prerequisites

- Elixir 1.14+ / Erlang/OTP 25+
- Git installed and accessible in PATH
- A Git repository for storing application specifications

## Installation

Add to your `mix.exs`:

```elixir
def deps do
  [
    {:bc_gitops, "~> 0.4.0"}
  ]
end
```

Add `bc_gitops` to your applications:

```elixir
def application do
  [
    extra_applications: [:logger, :bc_gitops]
  ]
end
```

## Step 1: Create Your GitOps Repository

Create a new Git repository to store your application specifications:

```bash
mkdir my-gitops-repo
cd my-gitops-repo
git init
mkdir apps
```

## Step 2: Define an Application

Create a specification file for each application you want to manage. Let's create one for a hypothetical `my_web_app`:

```bash
mkdir apps/my_web_app
```

bc_gitops supports three configuration formats. For Elixir projects, **YAML is recommended** for its clean syntax:

### Option A: YAML (`app.yaml`) - Recommended for Elixir

> **Note:** Requires `yamerl` dependency. Add `{:yamerl, "~> 0.10.0"}` to your deps.

Create `apps/my_web_app/app.yaml`:

```yaml
name: my_web_app
version: "1.0.0"

source:
  type: hex
  # Or for git:
  # type: git
  # url: https://github.com/myorg/my_web_app.git
  # ref: v1.0.0

env:
  port: 8080
  pool_size: 10

health:
  type: http
  port: 8080
  path: /health
  interval: 30000
  timeout: 5000

depends_on: []
```

### Option B: JSON (`app.json`)

> **Note:** Requires OTP 27+ for native JSON support.

Create `apps/my_web_app/app.json`:

```json
{
  "name": "my_web_app",
  "version": "1.0.0",
  "source": {
    "type": "hex"
  },
  "env": {
    "port": 8080,
    "pool_size": 10
  },
  "health": {
    "type": "http",
    "port": 8080,
    "path": "/health",
    "interval": 30000,
    "timeout": 5000
  },
  "depends_on": []
}
```

### Option C: Erlang Terms (`app.config`)

Create `apps/my_web_app/app.config`:

```erlang
#{
    name => my_web_app,
    version => <<"1.0.0">>,
    source => #{type => hex},
    env => #{port => 8080, pool_size => 10},
    health => #{
        type => http,
        port => 8080,
        path => <<"/health">>,
        interval => 30000,
        timeout => 5000
    },
    depends_on => []
}.
```

### Config File Priority

bc_gitops looks for config files in this order:
1. `app.config` (Erlang terms)
2. `app.yaml` / `app.yml` (YAML)
3. `app.json` (JSON)
4. `config.yaml` / `config.yml` / `config.json` / `config`

Commit and push:

```bash
git add .
git commit -m "Add my_web_app specification"
git remote add origin https://github.com/myorg/my-gitops-repo.git
git push -u origin main
```

## Step 3: Configure bc_gitops

Add configuration to your `config/config.exs`:

```elixir
config :bc_gitops,
  repo_url: "https://github.com/myorg/my-gitops-repo.git",
  local_path: "/var/lib/bc_gitops",
  branch: "main",
  reconcile_interval: 60_000,
  apps_dir: "apps",
  runtime_module: :bc_gitops_runtime_default
```

For development, you might want a shorter interval and local path:

```elixir
# config/dev.exs
config :bc_gitops,
  local_path: "_bc_gitops",
  reconcile_interval: 10_000
```

## Step 4: Start the Application

bc_gitops starts automatically when your application starts. It will:

1. Clone the repository (or pull if already cloned)
2. Parse all application specifications in `apps/`
3. Compare desired state with current state
4. Deploy/upgrade/remove applications as needed
5. Repeat every `reconcile_interval` milliseconds

## Step 5: Monitor and Operate

### Check Status

```elixir
{:ok, status} = :bc_gitops.status()
# %{status: :synced, last_commit: "abc123...", app_count: 5, healthy_count: 5}
```

### Trigger Manual Reconciliation

```elixir
:ok = :bc_gitops.reconcile()
```

### View States

```elixir
# Desired state (from git)
{:ok, desired} = :bc_gitops.get_desired_state()

# Current state (running)
{:ok, current} = :bc_gitops.get_current_state()

# Specific app
{:ok, app_state} = :bc_gitops.get_app_status(:my_web_app)
```

## Understanding Upgrades

### Why Restart on Upgrade?

Version upgrades restart the application rather than hot-reloading because several things in OTP cannot be updated at runtime:

1. **Application metadata** - `Application.get_key/2` reads from a cache populated at app start. Hot reload doesn't refresh this cache, so `:vsn`, `:description`, and custom keys return stale values.

2. **Plug/Phoenix routes** - Routes are compiled into the router module. New routes added in an upgrade won't be available without restarting.

3. **Supervision trees** - New child specs, changed restart strategies, or restructured supervisors require the supervisor to restart.

4. **Application config** - While `Application.put_env/3` can update values, many applications read config only at startup (e.g., in `start/2`).

For **same-version code changes** (e.g., tracking a `master` branch during development), hot reload works well because you're only updating module bytecode, not structural changes. Use `:bc_gitops_hot_reload` directly for this:

```elixir
# Reload changed modules only
{:ok, modules} = :bc_gitops_hot_reload.reload_changed_modules(:my_app)
```

## Deployment Workflow

Once bc_gitops is running, your deployment workflow becomes:

1. **Make changes** to your application specifications in git
2. **Commit and push** to the tracked branch
3. **Wait** for bc_gitops to detect changes (or trigger manually)
4. **Verify** the deployment via status API or telemetry

```bash
# Update version in apps/my_web_app/app.config
# Change: version => <<"1.0.0">> to version => <<"1.1.0">>

git add .
git commit -m "Upgrade my_web_app to 1.1.0"
git push
```

bc_gitops will automatically detect the change and upgrade the application.

## Telemetry Integration

bc_gitops emits telemetry events that you can subscribe to. Add this to your application supervisor:

```elixir
defmodule MyApp.GitOpsTelemetry do
  require Logger

  def setup do
    :telemetry.attach_many(
      "gitops-logger",
      [
        [:bc_gitops, :reconcile, :stop],
        [:bc_gitops, :deploy, :stop],
        [:bc_gitops, :upgrade, :stop],
        [:bc_gitops, :remove, :stop]
      ],
      &handle_event/4,
      nil
    )
  end

  def handle_event([:bc_gitops, action, :stop], measurements, metadata, _config) do
    Logger.info("GitOps #{action}: #{inspect(metadata)} (#{measurements[:duration]}ms)")
  end
end
```

Call `MyApp.GitOpsTelemetry.setup()` in your application start.

## Custom Runtime (Optional)

For most use cases, the default runtime works out of the box. If you need custom deployment logic, implement the `bc_gitops_runtime` behaviour:

```elixir
defmodule MyApp.GitOpsRuntime do
  @behaviour :bc_gitops_runtime

  @impl true
  def deploy(app_spec) do
    # Custom deployment logic
    :bc_gitops_runtime_default.deploy(app_spec)
  end

  @impl true
  def remove(app_name) do
    :bc_gitops_runtime_default.remove(app_name)
  end

  @impl true
  def upgrade(app_spec, old_version) do
    # Custom upgrade logic (e.g., blue-green)
    :bc_gitops_runtime_default.upgrade(app_spec, old_version)
  end

  @impl true
  def reconfigure(app_spec) do
    :bc_gitops_runtime_default.reconfigure(app_spec)
  end

  @impl true
  def get_current_state do
    :bc_gitops_runtime_default.get_current_state()
  end
end
```

Then configure it:

```elixir
config :bc_gitops, runtime_module: MyApp.GitOpsRuntime
```

## Next Steps

- Read the [API Reference](api.md) for the full API
- Read the [Runtime Implementation Guide](runtime_implementation.md) for advanced deployment strategies
- Configure [Git authentication](https://hexdocs.pm/bc_gitops) for private repositories
