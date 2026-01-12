# Plan: bc_gitops Example Applications

**Goal**: Create demonstration applications for a bc_gitops video tutorial

## Overview

Three example applications showcasing different bc_gitops capabilities:

1. **demo_web** - Elixir Phoenix app that **hosts bc_gitops** (the orchestrator)
2. **demo_counter** - Simple Erlang stateful app (managed by bc_gitops)
3. **demo_tui** - Rust TUI wrapped as OTP app (managed by bc_gitops)

Plus a **demo GitOps repository** that bc_gitops will watch.

## Architecture: demo_web as Host

```
┌─────────────────────────────────────────────────────────────┐
│ demo_web (Phoenix LiveView)                                 │
│   - Hosts bc_gitops as a dependency                         │
│   - Displays real-time dashboard of managed apps            │
│   - Shows telemetry events, deploy/upgrade status           │
│                                                             │
│   ┌─────────────────────────────────────────────────────┐   │
│   │ bc_gitops (running inside demo_web)                 │   │
│   │   - Watches: github.com/beam-campus/bc-gitops-demo  │   │
│   │   - Manages: demo_counter, demo_tui                 │   │
│   └─────────────────────────────────────────────────────┘   │
└─────────────────────────────────────────────────────────────┘
         │                              │
         ▼                              ▼
┌─────────────────┐          ┌─────────────────┐
│ demo_counter    │          │ demo_tui        │
│ (Erlang)        │          │ (Rust + Erlang) │
│ - HTTP API      │          │ - Terminal UI   │
│ - Stateful      │          │ - Port-based    │
└─────────────────┘          └─────────────────┘
```

**Key insight**: demo_web is NOT managed by bc_gitops - it's the host that runs bc_gitops. The other two apps ARE managed.

## Repository Structure

```
beam-campus/
├── bc-gitops                    # The library (existing)
├── bc-gitops-demo-repo          # GitOps spec repository (NEW)
│   └── apps/
│       ├── demo_counter/
│       │   └── app.config       # Managed by bc_gitops
│       └── demo_tui/
│           └── app.config       # Managed by bc_gitops
│       # NOTE: demo_web is NOT here - it's the host, not managed
├── bc-gitops-demo-web           # Phoenix host app (NEW) - runs bc_gitops
├── bc-gitops-demo-counter       # Erlang counter app (NEW) - managed
└── bc-gitops-demo-tui           # Rust TUI + Erlang wrapper (NEW) - managed
```

## Application 1: demo_counter (Erlang)

**Purpose**: Simplest possible stateful app to demonstrate deploy/upgrade/remove cycle.

### Features
- gen_server holding a counter value
- HTTP API (cowboy): GET /count, POST /increment, POST /reset
- Health endpoint: GET /health
- Implements `code_change/3` to preserve state during hot reload

### Demo Scenarios
1. Deploy v1.0.0 - counter starts at 0
2. Increment counter to 42
3. Upgrade to v1.1.0 (adds /decrement endpoint) - counter stays at 42!
4. Show hot reload preserved state
5. Remove application

### Files
```
bc-gitops-demo-counter/
├── src/
│   ├── demo_counter_app.erl
│   ├── demo_counter_sup.erl
│   ├── demo_counter_server.erl   # gen_server with code_change/3
│   └── demo_counter_handler.erl  # cowboy handler
├── rebar.config
└── README.md
```

## Application 2: demo_web (Elixir/Phoenix) - THE HOST

**Purpose**: Host application that runs bc_gitops and provides a real-time dashboard.

**This is the main entry point for the demo** - you start demo_web, and it manages everything else.

### Features
- **Hosts bc_gitops** as a dependency - starts it on boot
- Phoenix LiveView dashboard showing:
  - Managed apps (demo_counter, demo_tui) with status
  - Real-time telemetry events (deploy, upgrade, remove)
  - Git sync status (last commit, sync state)
  - App health indicators
- Interactive controls:
  - Manual sync button
  - View app details
  - See deployment logs

### Architecture
```elixir
# mix.exs
defp deps do
  [
    {:bc_gitops, "~> 0.2.1"},
    {:phoenix, "~> 1.7"},
    {:phoenix_live_view, "~> 0.20"}
  ]
end

# config/config.exs
config :bc_gitops,
  repo_url: "https://github.com/beam-campus/bc-gitops-demo-repo.git",
  branch: "main",
  reconcile_interval: 30_000,
  runtime_module: :bc_gitops_runtime_default
```

### Demo Scenarios
1. Start demo_web - shows empty dashboard, bc_gitops starts watching git repo
2. Commit demo_counter to git repo - watch it appear in dashboard, get deployed
3. Interact with demo_counter API - show it's running
4. Upgrade demo_counter version in git - watch hot reload in dashboard
5. Add demo_tui to git - watch TUI appear in terminal
6. Remove demo_counter from git - watch it get stopped

### Telemetry Integration
```elixir
# Subscribe to bc_gitops telemetry events
defmodule DemoWeb.GitopsTelemetry do
  def attach do
    :telemetry.attach_many("demo-web-gitops", [
      [:bc_gitops, :reconcile, :start],
      [:bc_gitops, :reconcile, :stop],
      [:bc_gitops, :deploy, :stop],
      [:bc_gitops, :upgrade, :stop],
      [:bc_gitops, :remove, :stop]
    ], &handle_event/4, nil)
  end

  def handle_event(event, measurements, metadata, _config) do
    Phoenix.PubSub.broadcast(DemoWeb.PubSub, "gitops:events", {event, measurements, metadata})
  end
end
```

### Files
```
bc-gitops-demo-web/
├── lib/
│   ├── demo_web/
│   │   ├── application.ex         # Starts bc_gitops
│   │   ├── gitops_telemetry.ex    # Telemetry -> PubSub bridge
│   │   └── ...
│   └── demo_web_web/
│       ├── live/
│       │   ├── dashboard_live.ex  # Main dashboard
│       │   ├── app_card.ex        # App status component
│       │   └── event_log.ex       # Event stream component
│       └── ...
├── config/
│   ├── config.exs                 # bc_gitops config
│   └── runtime.exs
├── mix.exs                        # deps: bc_gitops, phoenix
└── README.md
```

## Application 3: demo_tui (Rust + Erlang wrapper)

**Purpose**: Show non-BEAM executable management via OTP `priv/` directory pattern.

### Key Concept: priv/ Directory

OTP releases automatically include the `priv/` directory of each application. This is the standard way to bundle non-Erlang assets (executables, shared libraries, static files). When bc_gitops fetches the package from hex.pm, the binary comes along automatically.

### Architecture
```
┌─────────────────────────────────────────┐
│ demo_tui (OTP Application)              │
│  ┌───────────────────────────────────┐  │
│  │ demo_tui_sup (supervisor)         │  │
│  │  ┌─────────────────────────────┐  │  │
│  │  │ demo_tui_port (gen_server)  │  │  │
│  │  │                             │  │  │
│  │  │  priv/demo_tui (Rust bin)   │  │  │
│  │  │  └─> stdin/stdout JSON      │  │  │
│  │  └─────────────────────────────┘  │  │
│  └───────────────────────────────────┘  │
└─────────────────────────────────────────┘
```

### How It Works

```erlang
%% demo_tui_port.erl
start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

init([]) ->
    Executable = get_executable(),
    Port = open_port({spawn_executable, Executable}, [
        {line, 1024}, binary, use_stdio
    ]),
    {ok, #{port => Port}}.

get_executable() ->
    PrivDir = code:priv_dir(demo_tui),
    Arch = erlang:system_info(system_architecture),
    Binary = case Arch of
        "x86_64-pc-linux" ++ _ -> "demo_tui-linux-x86_64";
        "aarch64-unknown-linux" ++ _ -> "demo_tui-linux-aarch64";
        "x86_64-apple-darwin" ++ _ -> "demo_tui-darwin-x86_64";
        "aarch64-apple-darwin" ++ _ -> "demo_tui-darwin-aarch64";
        _ -> error({unsupported_arch, Arch})
    end,
    filename:join(PrivDir, Binary).
```

### Build System

rebar3 pre-compile hook builds the Rust binary:

```erlang
%% rebar.config
{pre_hooks, [
    {compile, "cargo build --release --manifest-path native/Cargo.toml"},
    {compile, "mkdir -p priv && cp native/target/release/demo_tui priv/demo_tui-$(./scripts/arch.sh)"}
]}.

%% Include priv/ in hex package
{hex, [{files, ["src", "priv", "rebar.config", "README.md", "LICENSE"]}]}.
```

### Features
- Rust TUI using ratatui (terminal dashboard)
- Communicates with Erlang via stdin/stdout JSON
- Shows system metrics from BEAM VM
- Erlang supervisor handles port lifecycle (restarts on crash)

### Demo Scenarios
1. Deploy - TUI appears in terminal (binary fetched from hex.pm!)
2. Upgrade - New TUI version with more widgets
3. Crash the TUI - supervisor restarts it automatically
4. Remove - TUI cleanly exits

### Files
```
bc-gitops-demo-tui/
├── native/                    # Rust TUI source
│   ├── Cargo.toml
│   └── src/
│       └── main.rs           # ratatui app, JSON protocol
├── src/                      # Erlang wrapper
│   ├── demo_tui.app.src
│   ├── demo_tui_app.erl
│   ├── demo_tui_sup.erl
│   └── demo_tui_port.erl     # gen_server managing port
├── priv/                     # Precompiled binaries for hex.pm
│   ├── demo_tui-linux-x86_64
│   ├── demo_tui-linux-aarch64
│   ├── demo_tui-darwin-x86_64
│   └── demo_tui-darwin-aarch64
├── scripts/
│   └── arch.sh               # Outputs current arch string
├── rebar.config
└── README.md
```

### Hex.pm Distribution Strategy

Include precompiled binaries for all common platforms. The Erlang code selects the right one at runtime based on `erlang:system_info(system_architecture)`.

**Platforms to support:**
- linux-x86_64 (most servers, CI)
- linux-aarch64 (ARM servers, Raspberry Pi)
- darwin-x86_64 (Intel Macs)
- darwin-aarch64 (Apple Silicon)

## GitOps Demo Repository

This is the git repository that bc_gitops watches. It only contains specs for **managed** apps (not demo_web, which is the host).

```
bc-gitops-demo-repo/
├── apps/
│   ├── demo_counter/
│   │   └── app.config      # Erlang counter app
│   └── demo_tui/
│       └── app.config      # Rust TUI app
└── README.md
```

**Note**: demo_web is NOT in this repo - it's the host that runs bc_gitops.

### Example app.config (demo_counter)
```erlang
#{
    name => demo_counter,
    version => <<"1.0.0">>,
    source => #{
        type => hex
    },
    env => #{
        port => 8081,
        initial_count => 0
    },
    health => #{
        type => http,
        port => 8081,
        path => <<"/health">>
    },
    depends_on => []
}.
```

## Video Script Outline

### Part 1: Introduction (2 min)
- What is GitOps?
- Why bc_gitops for BEAM?
- Architecture overview (show SVG)

### Part 2: Setup & Start demo_web (3 min)
- Show the demo repo structure (empty apps/ folder)
- Show demo_web Phoenix app with bc_gitops dependency
- Start demo_web: `mix phx.server`
- Show dashboard - empty, but bc_gitops is watching the repo

### Part 3: Deploy demo_counter (5 min)
- Add demo_counter/app.config to git repo
- Commit and push
- Watch dashboard update in real-time:
  - "Reconciling..." status
  - "Deploying demo_counter..."
  - App card appears with "running" status
- Interact with counter API (curl)
- Show telemetry events in dashboard

### Part 4: Hot Code Upgrade (5 min)
- Increment counter to 42
- Edit git: change version 1.0.0 → 1.1.0
- Push to git
- Watch dashboard:
  - "Upgrading demo_counter..."
  - "Hot reload: 3 modules changed"
- Verify counter still shows 42!
- Explain code_change/3 callback

### Part 5: Non-BEAM Integration (5 min)
- Add demo_tui/app.config to git repo
- Commit and push
- Watch dashboard: "Deploying demo_tui..."
- TUI appears in terminal!
- Explain priv/ directory pattern
- Kill TUI manually - supervisor restarts it
- Upgrade TUI version - new widgets appear

### Part 6: Remove & Dependencies (4 min)
- Remove demo_counter from git
- Watch dashboard: "Removing demo_counter..."
- App card disappears
- Add it back with `depends_on => [demo_tui]`
- Show dependency ordering in dashboard

### Part 7: Conclusion (2 min)
- Recap: GitOps for BEAM without Kubernetes
- Show all the code is ~500 lines
- Link to hexdocs
- Call to action (star repo, try it out)

**Total: ~26 minutes**

## Implementation Order

1. [ ] Create bc-gitops-demo-repo (GitOps specs - initially empty apps/)
2. [ ] Create bc-gitops-demo-web (Phoenix host) **FIRST - this is the orchestrator**
   - [ ] Phoenix LiveView project with bc_gitops dependency
   - [ ] Dashboard LiveView with app cards
   - [ ] Telemetry -> PubSub bridge for real-time updates
   - [ ] Event log component
   - [ ] Manual sync button
   - [ ] **Does NOT get published to hex.pm** (it's the host, not a library)
3. [ ] Create bc-gitops-demo-counter (Erlang - managed app)
   - [ ] Basic gen_server with HTTP API
   - [ ] Implement code_change/3 for state preservation
   - [ ] Health endpoint
   - [ ] Publish to hex.pm
4. [ ] Create bc-gitops-demo-tui (Rust + Erlang - managed app)
   - [ ] Rust TUI with JSON protocol (ratatui)
   - [ ] Erlang port wrapper
   - [ ] Precompiled binaries in priv/
   - [ ] Publish to hex.pm
5. [ ] Add app.configs to demo-repo and test full flow
6. [ ] Record video
7. [ ] Publish video + blog post

## Open Questions

1. ~~**Hex publishing for Rust binaries**~~ **RESOLVED**: Use `priv/` directory with precompiled binaries for linux-x86_64, linux-aarch64, darwin-x86_64, darwin-aarch64. Runtime selection via `erlang:system_info(system_architecture)`.

2. **Demo environment**: Run in Docker? Local BEAM? Show terminal recording?
   - Recommendation: Local terminal with asciinema for clean recording

3. ~~**Should demo_web show bc_gitops internals?**~~ **RESOLVED**: Yes! demo_web IS the host application. It runs bc_gitops and provides a real-time dashboard showing all managed apps, telemetry events, and sync status.

## Success Criteria

- [ ] demo_web (host) working with bc_gitops dashboard
- [ ] demo_counter published to hex.pm, deployable via bc_gitops
- [ ] demo_tui published to hex.pm with precompiled binaries
- [ ] Demo repo ready with app.configs for counter and tui
- [ ] Full flow tested: deploy, upgrade (with state preservation), remove
- [ ] Video recorded and published
- [ ] Blog post written
- [ ] bc_gitops README updated with video link
