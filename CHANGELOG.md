# Changelog

All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.0.0/),
and this project adheres to [Semantic Versioning](https://semver.org/spec/v2.0.0.html).

## [Unreleased]

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

[Unreleased]: https://github.com/beam-campus/bc-gitops/compare/v0.3.1...HEAD
[0.3.1]: https://github.com/beam-campus/bc-gitops/compare/v0.3.0...v0.3.1
[0.3.0]: https://github.com/beam-campus/bc-gitops/compare/v0.2.1...v0.3.0
[0.2.1]: https://github.com/beam-campus/bc-gitops/compare/v0.2.0...v0.2.1
[0.2.0]: https://github.com/beam-campus/bc-gitops/compare/v0.1.0...v0.2.0
[0.1.0]: https://github.com/beam-campus/bc-gitops/releases/tag/v0.1.0
