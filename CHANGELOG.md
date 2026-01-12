# Changelog

All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.0.0/),
and this project adheres to [Semantic Versioning](https://semver.org/spec/v2.0.0.html).

## [Unreleased]

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

[Unreleased]: https://github.com/beam-campus/bc-gitops/compare/v0.1.0...HEAD
[0.1.0]: https://github.com/beam-campus/bc-gitops/releases/tag/v0.1.0
