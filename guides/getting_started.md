# Getting Started with bc_gitops

This guide walks you through setting up bc_gitops to manage OTP applications using GitOps principles.

## What is GitOps?

GitOps is an operational framework where:

1. **Git is the source of truth** - The desired state of your system is stored in a Git repository
2. **Declarative configuration** - You describe *what* you want, not *how* to achieve it
3. **Automatic reconciliation** - The system continuously compares desired vs actual state and takes corrective actions

bc_gitops brings this pattern to the BEAM ecosystem, allowing you to manage OTP applications the same way Flux or ArgoCD manage Kubernetes workloads.

## Prerequisites

- Erlang/OTP 25+ or Elixir 1.14+
- Git installed and accessible in PATH
- A Git repository for storing application specifications

## Installation

### For Erlang (rebar3)

Add to your `rebar.config`:

```erlang
{deps, [
    {bc_gitops, "0.1.0"}
]}.
```

### For Elixir (Mix)

Add to your `mix.exs`:

```elixir
def deps do
  [
    {:bc_gitops, "~> 0.1.0"}
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

Create `apps/my_web_app/app.config`:

```erlang
#{
    %% Application name (must match the OTP application name)
    name => my_web_app,

    %% Version to deploy
    version => <<"1.0.0">>,

    %% Where to get the application from
    source => #{
        type => hex  %% From hex.pm
        %% Or for git:
        %% type => git,
        %% url => <<"https://github.com/myorg/my_web_app.git">>,
        %% ref => <<"v1.0.0">>
    },

    %% Application environment (passed to application:set_env)
    env => #{
        port => 8080,
        pool_size => 10
    },

    %% Health check configuration (optional)
    health => #{
        type => http,
        port => 8080,
        path => <<"/health">>,
        interval => 30000,  %% Check every 30 seconds
        timeout => 5000     %% Timeout after 5 seconds
    },

    %% Dependencies (other managed apps that must start first)
    depends_on => []
}.
```

Commit and push:

```bash
git add .
git commit -m "Add my_web_app specification"
git remote add origin https://github.com/myorg/my-gitops-repo.git
git push -u origin main
```

## Step 3: Implement a Runtime Module

bc_gitops needs to know *how* to deploy applications. You provide this by implementing the `bc_gitops_runtime` behaviour.

For simple cases, you can use the built-in `bc_gitops_runtime_default`, but for production you'll want a custom implementation.

Create `src/my_app_runtime.erl`:

```erlang
-module(my_app_runtime).
-behaviour(bc_gitops_runtime).

-include_lib("bc_gitops/include/bc_gitops.hrl").

-export([deploy/1, remove/1, upgrade/2, reconfigure/1, get_current_state/0]).

%% @doc Deploy a new application
deploy(#app_spec{name = Name, version = Version, env = Env}) ->
    %% 1. Download/fetch the application (if needed)
    %% 2. Set environment variables
    set_env(Name, Env),

    %% 3. Start the application
    case application:ensure_all_started(Name) of
        {ok, _} ->
            {ok, #app_state{
                name = Name,
                version = Version,
                status = running,
                started_at = calendar:universal_time(),
                health = unknown,
                env = Env
            }};
        {error, Reason} ->
            {error, {start_failed, Reason}}
    end.

%% @doc Remove (stop) an application
remove(Name) ->
    case application:stop(Name) of
        ok -> ok;
        {error, {not_started, _}} -> ok;
        {error, Reason} -> {error, Reason}
    end.

%% @doc Upgrade an application to a new version
upgrade(AppSpec, _OldVersion) ->
    %% Simple strategy: stop and redeploy
    %% For zero-downtime, implement hot code upgrades
    remove(AppSpec#app_spec.name),
    deploy(AppSpec).

%% @doc Update application configuration without restart
reconfigure(#app_spec{name = Name, version = Version, env = NewEnv}) ->
    set_env(Name, NewEnv),
    {ok, #app_state{
        name = Name,
        version = Version,
        status = running,
        started_at = calendar:universal_time(),
        health = unknown,
        env = NewEnv
    }}.

%% @doc Get the current state of all managed applications
get_current_state() ->
    %% Query your application registry/supervisor
    {ok, #{}}.

%% Internal helper
set_env(App, Env) ->
    maps:foreach(fun(K, V) ->
        application:set_env(App, K, V)
    end, Env).
```

## Step 4: Configure bc_gitops

Add configuration to your `sys.config` (or `config/config.exs` for Elixir):

### Erlang (sys.config)

```erlang
[
    {bc_gitops, [
        %% Required: Git repository URL
        {repo_url, "https://github.com/myorg/my-gitops-repo.git"},

        %% Optional: Local clone path (default: /var/lib/bc_gitops)
        {local_path, "/var/lib/bc_gitops"},

        %% Optional: Branch to track (default: main)
        {branch, "main"},

        %% Optional: Reconcile interval in ms (default: 60000)
        {reconcile_interval, 60000},

        %% Optional: Directory containing app specs (default: apps)
        {apps_dir, "apps"},

        %% Required: Your runtime implementation
        {runtime_module, my_app_runtime}
    ]}
].
```

### Elixir (config.exs)

```elixir
config :bc_gitops,
  repo_url: "https://github.com/myorg/my-gitops-repo.git",
  local_path: "/var/lib/bc_gitops",
  branch: "main",
  reconcile_interval: 60_000,
  apps_dir: "apps",
  runtime_module: MyAppRuntime
```

## Step 5: Start the Application

Add `bc_gitops` to your application's dependencies and start it:

```erlang
application:ensure_all_started(bc_gitops).
```

bc_gitops will:

1. Clone the repository (or pull if already cloned)
2. Parse all application specifications in `apps/`
3. Compare desired state with current state
4. Deploy/upgrade/remove applications as needed
5. Repeat every `reconcile_interval` milliseconds

## Step 6: Monitor and Operate

### Check Status

```erlang
{ok, Status} = bc_gitops:status().
%% #{status => synced,
%%   last_commit => <<"abc123...">>,
%%   app_count => 5,
%%   healthy_count => 5}
```

### Trigger Manual Reconciliation

```erlang
bc_gitops:reconcile().
```

### View States

```erlang
%% Desired state (from git)
{ok, Desired} = bc_gitops:get_desired_state().

%% Current state (running)
{ok, Current} = bc_gitops:get_current_state().

%% Specific app
{ok, AppState} = bc_gitops:get_app_status(my_web_app).
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

## Next Steps

- Read the [Runtime Implementation Guide](runtime_implementation.md) for advanced deployment strategies
- Set up [Telemetry handlers](https://hexdocs.pm/bc_gitops/readme.html#telemetry-events) for monitoring
- Configure [Git authentication](https://hexdocs.pm/bc_gitops/readme.html#git-authentication) for private repositories
