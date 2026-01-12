# Changelog

All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.0.0/),
and this project adheres to [Semantic Versioning](https://semver.org/spec/v2.0.0.html).

## [Unreleased]

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

[Unreleased]: https://github.com/beam-campus/bc-gitops/compare/v0.2.1...HEAD
[0.2.1]: https://github.com/beam-campus/bc-gitops/compare/v0.2.0...v0.2.1
[0.2.0]: https://github.com/beam-campus/bc-gitops/compare/v0.1.0...v0.2.0
[0.1.0]: https://github.com/beam-campus/bc-gitops/releases/tag/v0.1.0
