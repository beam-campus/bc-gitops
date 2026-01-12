# Contributing to bc_gitops

Thank you for your interest in contributing to bc_gitops! This document provides guidelines and instructions for contributing.

## Code of Conduct

By participating in this project, you agree to maintain a respectful and inclusive environment. Be kind to others, welcome newcomers, and focus on constructive feedback.

## How to Contribute

### Reporting Bugs

Before reporting a bug:

1. Check the [existing issues](https://github.com/beam-campus/bc-gitops/issues) to avoid duplicates
2. Ensure you're using the latest version
3. Collect relevant information:
   - Erlang/OTP version (`erl -version`)
   - bc_gitops version
   - Operating system
   - Steps to reproduce
   - Expected vs actual behavior

Create a new issue with the **Bug Report** template.

### Suggesting Features

We welcome feature suggestions! Before submitting:

1. Check existing issues for similar suggestions
2. Consider if the feature fits the project's scope (BEAM-native GitOps)
3. Think about backwards compatibility

Create a new issue with the **Feature Request** template.

### Pull Requests

#### Setting Up Development Environment

1. Fork the repository
2. Clone your fork:
   ```bash
   git clone https://github.com/YOUR_USERNAME/bc-gitops.git
   cd bc-gitops
   ```
3. Install dependencies:
   ```bash
   rebar3 deps
   ```
4. Run tests to ensure everything works:
   ```bash
   rebar3 eunit
   ```

#### Making Changes

1. Create a feature branch:
   ```bash
   git checkout -b feature/my-feature
   # or
   git checkout -b fix/bug-description
   ```

2. Make your changes, following the coding standards below

3. Add tests for new functionality

4. Run the full test suite:
   ```bash
   rebar3 eunit
   ```

5. Run static analysis:
   ```bash
   rebar3 dialyzer
   rebar3 xref
   ```

6. Commit your changes:
   ```bash
   git commit -m "feat: add new feature X"
   ```

7. Push and create a PR:
   ```bash
   git push origin feature/my-feature
   ```

#### Commit Message Format

We follow [Conventional Commits](https://www.conventionalcommits.org/):

```
<type>(<scope>): <description>

[optional body]

[optional footer]
```

**Types:**
- `feat`: New feature
- `fix`: Bug fix
- `docs`: Documentation only
- `style`: Code style (formatting, etc.)
- `refactor`: Code change that neither fixes a bug nor adds a feature
- `test`: Adding or updating tests
- `chore`: Maintenance tasks

**Examples:**
```
feat(reconciler): add topological sort for dependencies
fix(git): handle authentication failures gracefully
docs(readme): add telemetry event documentation
test(parser): add JSON config parsing tests
```

## Coding Standards

### Erlang Style

- Use 4-space indentation
- Maximum line length: 100 characters
- Follow OTP design principles
- Add type specs (`-spec`) for all exported functions
- Add documentation (`@doc`) for all exported functions

### Module Structure

```erlang
%%% @doc Module description.
%%%
%%% Detailed explanation of what this module does.
%%% @end
-module(module_name).

-behaviour(gen_server).  %% If applicable

%% API exports
-export([public_function/1]).

%% Callback exports
-export([init/1, handle_call/3]).

%% Internal exports (if needed for testing)
-ifdef(TEST).
-export([internal_function/1]).
-endif.

%% Macros and records
-define(TIMEOUT, 5000).
-record(state, {field :: type()}).

%% Type exports
-export_type([my_type/0]).

-type my_type() :: term().

%%% ============================================================================
%%% API
%%% ============================================================================

%% @doc Description of what this function does.
-spec public_function(Arg :: term()) -> {ok, Result :: term()} | {error, Reason :: term()}.
public_function(Arg) ->
    internal_function(Arg).

%%% ============================================================================
%%% Internal functions
%%% ============================================================================

-spec internal_function(term()) -> term().
internal_function(Arg) ->
    Arg.
```

### Testing

- Place tests in `test/` directory
- Name test modules `*_tests.erl`
- Use EUnit for unit tests
- Mock external dependencies with `meck`
- Aim for high coverage of public APIs

Example test structure:

```erlang
-module(my_module_tests).

-include_lib("eunit/include/eunit.hrl").

%% Test fixtures
setup() ->
    %% Setup code
    ok.

cleanup(_) ->
    %% Cleanup code
    ok.

%% Individual tests
my_function_returns_ok_test() ->
    ?assertEqual(ok, my_module:my_function()).

%% Test generators for setup/cleanup
my_test_() ->
    {setup,
        fun setup/0,
        fun cleanup/1,
        [
            ?_assertEqual(expected, actual)
        ]
    }.
```

### Documentation

- Document all exported functions with `@doc`
- Use `@since` for version tracking
- Include examples in documentation
- Keep README.md up to date
- Add guides for complex features

## Review Process

1. All PRs require at least one approval
2. CI must pass (tests, dialyzer, xref)
3. Documentation must be updated for user-facing changes
4. CHANGELOG.md must be updated for notable changes

## Release Process

Releases are managed by maintainers:

1. Update version in `src/bc_gitops.app.src`
2. Update CHANGELOG.md
3. Create a git tag: `git tag v0.1.0`
4. Push: `git push && git push --tags`
5. Publish to hex.pm: `rebar3 hex publish`

## Getting Help

- Open a [Discussion](https://github.com/beam-campus/bc-gitops/discussions) for questions
- Join the BEAM community on [Slack](https://elixir-lang.slack.com/) or [Discord](https://discord.gg/erlang)
- Check the [documentation](https://hexdocs.pm/bc_gitops)

## License

By contributing to bc_gitops, you agree that your contributions will be licensed under the MIT License.
