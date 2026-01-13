# Changelog

All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.0.0/),
and this project adheres to [Semantic Versioning](https://semver.org/spec/v2.0.0.html).

## [Unreleased]

## [0.6.3] - 2026-01-13

### Changed

- **Documentation**: Updated `guides/isolated_vm_deployment.md` with Macula Platform Integration section
  - Explains automatic delegation to macula when available
  - Documents delegated functions table
  - Links to Macula Cluster API Guide

## [0.6.2] - 2026-01-13

### Added

- **Node monitoring for isolated VMs** (Phase 3)
  - Reconciler subscribes to `net_kernel:monitor_nodes/1` on startup
  - Handles `nodeup` and `nodedown` messages automatically
  - Updates app states when their node disconnects
  - Emits `[:bc_gitops, :cluster, :node_up]` and `[:bc_gitops, :cluster, :node_down]` telemetry

### Changed

- **bc_gitops_reconciler**:
  - Now monitors cluster node events for isolated VMs
  - Apps on disconnected nodes are marked as `stopped`/`unhealthy`
  - Apps on reconnected nodes have their status re-verified via RPC

- **bc_gitops_cluster**: Macula integration (optional dependency)
  - Delegates to `macula` module when available for clustering operations
  - Falls back to local implementation when macula is not present
  - Delegated functions: `ensure_distributed/0`, `get_cookie/0`, `set_cookie/1`, `monitor_nodes/0`, `unmonitor_nodes/0`
  - bc_gitops remains usable standalone without macula

## [0.6.1] - 2026-01-13

### Added

- **Reconciler integration for isolated VMs** (Phase 2)
  - Runtime selection based on `isolation` field in app_spec
  - Automatic dispatch to `bc_gitops_runtime_isolated` for `isolation => vm`
  - State merging from both embedded and isolated runtimes
  - Telemetry events now include `isolation` mode and selected runtime

### Changed

- **bc_gitops_reconciler**:
  - `apply_action/2` now selects runtime based on `isolation` field
  - `get_runtime_state/2` merges states from default and isolated runtimes
  - Manual operations (`do_deploy`, `do_remove`, `do_upgrade`) respect isolation mode

### Fixed

- **bc_gitops_runtime_default**: Added missing `isolation = embedded` field to app_state records
- **bc_gitops_cluster**: Fixed `extra_args` default value in vm_config record
- **bc_gitops_runtime_isolated**: Changed unreachable `degraded` status to `failed`
- **bc_gitops_vm_spawner**: Simplified `shell_escape/1` spec to match actual usage

## [0.6.0] - 2026-01-13

### Added

- **Isolated VM deployment** - Run guest applications in separate BEAM VMs
  - New `isolation` field in app_spec: `embedded` (default) or `vm`
  - New `vm_config` record for resource limits (memory, schedulers)
  - Crash isolation: guest crashes don't affect host
  - Auto-clustering via Erlang distribution
  - Phoenix.PubSub works automatically across nodes

- **New modules**:
  - `bc_gitops_cluster` - Erlang distribution and cookie management
  - `bc_gitops_vm_spawner` - Spawn and manage separate BEAM VM processes
  - `bc_gitops_runtime_isolated` - Runtime behaviour for isolated VMs

- **New telemetry events**:
  - `[:bc_gitops, :vm, :spawn_start]` / `[:bc_gitops, :vm, :spawn_stop]`
  - `[:bc_gitops, :vm, :stop_start]` / `[:bc_gitops, :vm, :stop_stop]`
  - `[:bc_gitops, :cluster, :node_up]` / `[:bc_gitops, :cluster, :node_down]`

- **Documentation**:
  - `guides/isolated_vm_deployment.md` - Comprehensive operator guide
  - `assets/isolated_vm_architecture.svg` - Architecture diagram

### Changed

- **app_spec record**: Added `isolation` and `vm_config` fields
- **app_state record**: Added `isolation`, `node`, and `os_pid` fields for tracking isolated VMs
- **Parser**: Now handles `isolation` and `vm_config` configuration

### Example Configuration

```erlang
#{
    name => demo_uptime,
    version => <<"0.3.0">>,
    source => #{type => hex},
    isolation => vm,  %% Run in separate VM
    vm_config => #{
        memory_limit => 512,     %% 512 MB max
        scheduler_limit => 2     %% 2 schedulers
    },
    env => #{http_port => 8083}
}.
```

## [0.5.0] - 2026-01-13

### Added

- **Icon and description fields** in app config schema
  - Apps can specify `icon` (url, base64, or identicon type) and `description`
  - Auto-generated identicons based on app name when icon not specified
  - `bc_gitops_identicon` module generates deterministic SVG identicons
  - `to_data_uri/1,2` for embedding icons directly in HTML

- **Module purging on upgrade** (`bc_gitops_workspace`)
  - Properly purges old modules before removing code paths
  - Prevents stale code from persisting after hot upgrades
  - New telemetry event: `[:bc_gitops, :code, :purge]`

### Fixed

- **Hex package version fetching**: Now uses app version as package ref
  - Previously always fetched `">= 0.0.0"` (latest version)
  - Now fetches exact version specified in app config

## [0.4.1] - 2026-01-13

### Fixed

- **Git package fetching**: Handle existing directories during fetch
  - Previously, `fetch_git_package` always attempted `git clone`, failing when
    the target directory already existed (e.g., after server restart)
  - Now checks if directory exists and is a git repo:
    - If git repo exists: does `git fetch` + `git checkout` to update
    - If directory exists but not git repo: removes and clones fresh
    - If directory doesn't exist: clones normally
  - Fixes "destination path already exists" errors during reconciliation

## [0.4.0] - 2026-01-13

### Added

- **YAML config file support** (`.yaml`, `.yml`)
  - Requires optional `yamerl` dependency
  - Full support for all app spec fields
  - Automatic conversion of YAML keys to atoms

- **Enhanced JSON support**
  - Uses OTP 27+ native `json` module
  - Clear error messages for older OTP versions

### Changed

- **Config file search order** now includes YAML:
  1. `app.config` (Erlang terms)
  2. `app.yaml` / `app.yml` (YAML)
  3. `app.json` (JSON)
  4. `config.*` variants

- **Documentation updates**:
  - Added YAML and JSON examples to getting started guides
  - Added "App Config File Formats" section to API reference
  - Documented optional dependencies for YAML/JSON

## [0.3.1] - 2026-01-13

### Added

- **Elixir Getting Started Guide** (`guides/getting_started_elixir.md`)
  - Full walkthrough for Elixir users
  - Elixir-specific configuration examples
  - Telemetry setup with Elixir module
  - Custom runtime implementation in Elixir

### Changed

- **Documentation improvements**:
  - Added "Why Restart on Upgrade?" section to getting started guides
  - Updated architecture.svg to v0.3.0
  - Clarified hot_reload_flow.svg is for same-version changes
  - Updated runtime_implementation.md with v0.3.0+ notes
  - Renamed "Getting Started" to "Getting Started (Erlang)" in navigation

## [0.3.0] - 2026-01-12

### Changed

- **bc_gitops_runtime_default**: Version upgrades now restart the application
  - Ensures clean initialization (routes, supervisors, app metadata)
  - Fixes issue where `application:get_key/2` returned stale cached values
  - Hot code reload still available for same-version code changes via `bc_gitops_hot_reload`

- **Upgrade flow**: Always fetches fresh code for upgrades
  - Deletes existing workspace before fetching new version
  - Ensures clean build with correct ref/tag

### Added

- **Detailed telemetry events** for build pipeline visibility:
  - `[:bc_gitops, :git, :clone_start]` / `[:bc_gitops, :git, :clone_stop]`
  - `[:bc_gitops, :deps, :start]` / `[:bc_gitops, :deps, :stop]`
  - `[:bc_gitops, :build, :start]` / `[:bc_gitops, :build, :stop]`
  - `[:bc_gitops, :code, :load]`
  - All events include app name, tool used, and success/failure status

### Fixed

- Handle timeout gracefully in `bc_gitops:status/0` call

## [0.2.1] - 2026-01-12

### Added

- **guides/api.md**: Complete API quick-reference guide
  - Core module API (status, state, manual operations)
  - Runtime module callbacks
  - Workspace and hot reload APIs
  - Record definitions and telemetry events

### Changed

- **assets/architecture.svg**: Updated to show v0.2.0 modules
  - Added bc_gitops_workspace and bc_gitops_hot_reload
  - Added hex.pm external source and build tools

- **assets/hot_reload_flow.svg**: New diagram showing upgrade flow
  - Suspend -> reload -> resume sequence
  - MD5 comparison for change detection
  - Fallback to restart on failure

## [0.2.0] - 2026-01-12

### Added

- **bc_gitops_workspace**: Package fetching and compilation workspace
  - Fetches packages from hex.pm via rebar3 (Erlang) or mix (Elixir)
  - Clones and compiles git repositories
  - Manages code paths automatically
  - Supports rebar3, mix, and erlang.mk project types

- **bc_gitops_hot_reload**: Hot code reloading utilities
  - Module reloading with `code:soft_purge/1` + `code:load_file/1`
  - Process suspension/resumption for stateful upgrades
  - Change detection via beam file MD5 comparison
  - Coordinated upgrade across application modules

### Changed

- **bc_gitops_runtime_default**: Now fully functional out of the box
  - Actually fetches and compiles packages (was just a stub before)
  - Supports hot code upgrades with fallback to restart
  - Properly manages application environment and state

## [0.1.0] - 2026-01-12

### Added

- Initial release of bc_gitops
- Core reconciler (`bc_gitops_reconciler`) with configurable interval
- Git operations (`bc_gitops_git`) for clone, pull, and commit queries
- Configuration parser (`bc_gitops_parser`) supporting Erlang term and JSON formats
- Runtime behaviour (`bc_gitops_runtime`) for pluggable deployment strategies
- Default runtime implementation (`bc_gitops_runtime_default`) using OTP application management
- Telemetry events for reconciliation, deployment, and upgrade operations
- Comprehensive API (`bc_gitops`) for status queries and manual operations
- Full documentation with examples

[Unreleased]: https://github.com/beam-campus/bc-gitops/compare/v0.6.3...HEAD
[0.6.3]: https://github.com/beam-campus/bc-gitops/compare/v0.6.2...v0.6.3
[0.6.2]: https://github.com/beam-campus/bc-gitops/compare/v0.6.1...v0.6.2
[0.6.1]: https://github.com/beam-campus/bc-gitops/compare/v0.6.0...v0.6.1
[0.6.0]: https://github.com/beam-campus/bc-gitops/compare/v0.5.0...v0.6.0
[0.5.0]: https://github.com/beam-campus/bc-gitops/compare/v0.4.1...v0.5.0
[0.4.1]: https://github.com/beam-campus/bc-gitops/compare/v0.4.0...v0.4.1
[0.4.0]: https://github.com/beam-campus/bc-gitops/compare/v0.3.1...v0.4.0
[0.3.1]: https://github.com/beam-campus/bc-gitops/compare/v0.3.0...v0.3.1
[0.3.0]: https://github.com/beam-campus/bc-gitops/compare/v0.2.1...v0.3.0
[0.2.1]: https://github.com/beam-campus/bc-gitops/compare/v0.2.0...v0.2.1
[0.2.0]: https://github.com/beam-campus/bc-gitops/compare/v0.1.0...v0.2.0
[0.1.0]: https://github.com/beam-campus/bc-gitops/releases/tag/v0.1.0
