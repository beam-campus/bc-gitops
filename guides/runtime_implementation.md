# Runtime Implementation Guide

The runtime module is the heart of bc_gitops customization. It defines *how* applications are deployed, upgraded, and removed. This guide covers different implementation strategies from simple to advanced.

> **Note (v0.3.0+):** The default runtime (`bc_gitops_runtime_default`) now **restarts** applications on version upgrades rather than hot-reloading. This ensures clean initialization of routes, supervisors, and application metadata. For same-version code changes (e.g., tracking a branch), you can still use `bc_gitops_hot_reload` directly.

## The bc_gitops_runtime Behaviour

Every runtime must implement the `bc_gitops_runtime` behaviour:

```erlang
-callback deploy(AppSpec :: #app_spec{}) ->
    {ok, #app_state{}} | {error, term()}.

-callback remove(AppName :: atom()) ->
    ok | {error, term()}.

-callback upgrade(AppSpec :: #app_spec{}, OldVersion :: binary()) ->
    {ok, #app_state{}} | {error, term()}.

-callback reconfigure(AppSpec :: #app_spec{}) ->
    {ok, #app_state{}} | {error, term()}.

%% Optional
-callback get_current_state() ->
    {ok, #{atom() => #app_state{}}} | {error, term()}.
```

## Strategy 1: Simple Start/Stop (Default)

The simplest strategy stops the old version and starts the new one. This is what `bc_gitops_runtime_default` does for version upgrades (v0.3.0+):

```erlang
-module(simple_runtime).
-behaviour(bc_gitops_runtime).

-include_lib("bc_gitops/include/bc_gitops.hrl").

-export([deploy/1, remove/1, upgrade/2, reconfigure/1]).

deploy(#app_spec{name = Name, version = Version, env = Env}) ->
    set_env(Name, Env),
    case application:ensure_all_started(Name) of
        {ok, _} ->
            {ok, make_state(Name, Version, Env)};
        {error, Reason} ->
            {error, {start_failed, Reason}}
    end.

remove(Name) ->
    application:stop(Name).

upgrade(AppSpec, _OldVersion) ->
    %% Stop old, start new
    remove(AppSpec#app_spec.name),
    deploy(AppSpec).

reconfigure(#app_spec{name = Name, version = Version, env = Env}) ->
    set_env(Name, Env),
    {ok, make_state(Name, Version, Env)}.

%% Helpers
set_env(App, Env) ->
    maps:foreach(fun(K, V) -> application:set_env(App, K, V) end, Env).

make_state(Name, Version, Env) ->
    #app_state{
        name = Name,
        version = Version,
        status = running,
        started_at = calendar:universal_time(),
        health = unknown,
        env = Env
    }.
```

**Pros:**
- Simple to implement
- Works with any OTP application
- Clean initialization (routes, supervisors, metadata all fresh)

**Cons:**
- Brief downtime during upgrades
- Connections are dropped

> For same-version code changes where you want zero-downtime, use `bc_gitops_hot_reload` directly.

## Strategy 2: Hot Code Upgrade

OTP's release handler enables zero-downtime upgrades. This requires proper `.appup` files.

```erlang
-module(hot_upgrade_runtime).
-behaviour(bc_gitops_runtime).

-include_lib("bc_gitops/include/bc_gitops.hrl").

-export([deploy/1, remove/1, upgrade/2, reconfigure/1]).

deploy(AppSpec) ->
    %% For new deployments, use standard start
    simple_runtime:deploy(AppSpec).

remove(Name) ->
    simple_runtime:remove(Name).

upgrade(#app_spec{name = Name, version = NewVersion} = AppSpec, OldVersion) ->
    %% Try hot upgrade first
    case attempt_hot_upgrade(Name, OldVersion, NewVersion) of
        ok ->
            {ok, make_state(AppSpec)};
        {error, no_appup} ->
            %% Fall back to restart
            logger:warning("No appup for ~p, falling back to restart", [Name]),
            simple_runtime:upgrade(AppSpec, OldVersion);
        {error, Reason} ->
            {error, {hot_upgrade_failed, Reason}}
    end.

reconfigure(AppSpec) ->
    simple_runtime:reconfigure(AppSpec).

%% Hot upgrade using release_handler
attempt_hot_upgrade(Name, OldVsn, NewVsn) ->
    RelDir = code:lib_dir(Name),
    AppupFile = filename:join([RelDir, "ebin", atom_to_list(Name) ++ ".appup"]),

    case filelib:is_file(AppupFile) of
        false ->
            {error, no_appup};
        true ->
            %% Load the new version
            case release_handler:upgrade_app(Name, RelDir) of
                {ok, _} -> ok;
                Error -> Error
            end
    end.

make_state(#app_spec{name = N, version = V, env = E}) ->
    #app_state{name = N, version = V, status = running,
               started_at = calendar:universal_time(),
               health = unknown, env = E}.
```

**Pros:**
- Zero downtime
- Connections preserved
- State preserved

**Cons:**
- Requires `.appup` files
- Complex to test
- Not all changes can be hot-upgraded

## Strategy 3: Blue-Green Deployment

Run both versions simultaneously, then switch traffic:

```erlang
-module(blue_green_runtime).
-behaviour(bc_gitops_runtime).

-include_lib("bc_gitops/include/bc_gitops.hrl").

-export([deploy/1, remove/1, upgrade/2, reconfigure/1]).

%% State tracking for blue/green slots
-define(REGISTRY, blue_green_registry).

deploy(#app_spec{name = Name} = AppSpec) ->
    %% Deploy to "blue" slot
    case deploy_to_slot(AppSpec, blue) of
        {ok, State} ->
            %% Register as active
            register_active(Name, blue),
            {ok, State};
        Error ->
            Error
    end.

remove(Name) ->
    %% Remove from both slots
    remove_from_slot(Name, blue),
    remove_from_slot(Name, green),
    unregister_active(Name),
    ok.

upgrade(#app_spec{name = Name} = AppSpec, OldVersion) ->
    %% Get current active slot
    CurrentSlot = get_active_slot(Name),
    NewSlot = other_slot(CurrentSlot),

    %% Deploy new version to inactive slot
    case deploy_to_slot(AppSpec, NewSlot) of
        {ok, State} ->
            %% Health check the new deployment
            case health_check(Name, NewSlot) of
                healthy ->
                    %% Switch traffic
                    switch_traffic(Name, NewSlot),
                    register_active(Name, NewSlot),
                    %% Drain and stop old slot
                    drain_slot(Name, CurrentSlot),
                    remove_from_slot(Name, CurrentSlot),
                    {ok, State};
                unhealthy ->
                    %% Rollback - remove failed deployment
                    remove_from_slot(Name, NewSlot),
                    {error, {health_check_failed, NewSlot}}
            end;
        Error ->
            Error
    end.

reconfigure(AppSpec) ->
    %% Reconfigure active slot
    Name = AppSpec#app_spec.name,
    Slot = get_active_slot(Name),
    reconfigure_slot(AppSpec, Slot).

%% Slot management (simplified - use proper supervision in production)
deploy_to_slot(#app_spec{name = Name} = AppSpec, Slot) ->
    SlotName = slot_name(Name, Slot),
    %% Start under a slot-specific supervisor
    %% This is simplified - real implementation needs proper supervision
    simple_runtime:deploy(AppSpec#app_spec{name = SlotName}).

remove_from_slot(Name, Slot) ->
    SlotName = slot_name(Name, Slot),
    simple_runtime:remove(SlotName).

slot_name(Name, Slot) ->
    list_to_atom(atom_to_list(Name) ++ "_" ++ atom_to_list(Slot)).

other_slot(blue) -> green;
other_slot(green) -> blue.

%% Registry operations (use ETS or persistent_term in production)
register_active(Name, Slot) ->
    persistent_term:put({?REGISTRY, Name}, Slot).

get_active_slot(Name) ->
    persistent_term:get({?REGISTRY, Name}, blue).

unregister_active(Name) ->
    persistent_term:erase({?REGISTRY, Name}).

%% These would integrate with your load balancer / service mesh
switch_traffic(_Name, _Slot) -> ok.
drain_slot(_Name, _Slot) -> timer:sleep(5000).  %% Wait for connections to drain
health_check(_Name, _Slot) -> healthy.
reconfigure_slot(AppSpec, _Slot) -> simple_runtime:reconfigure(AppSpec).
```

**Pros:**
- Zero downtime
- Easy rollback
- Can verify before switching

**Cons:**
- Requires 2x resources during upgrade
- Complex state management
- Need load balancer integration

## Strategy 4: Release-Based Deployment

For applications distributed as OTP releases:

```erlang
-module(release_runtime).
-behaviour(bc_gitops_runtime).

-include_lib("bc_gitops/include/bc_gitops.hrl").

-export([deploy/1, remove/1, upgrade/2, reconfigure/1]).

-define(RELEASES_DIR, "/opt/releases").

deploy(#app_spec{name = Name, version = Version, source = Source} = AppSpec) ->
    %% Download release tarball
    case download_release(Name, Version, Source) of
        {ok, TarPath} ->
            %% Extract and install
            case install_release(Name, Version, TarPath) of
                ok ->
                    %% Start the release
                    start_release(Name, Version, AppSpec);
                Error ->
                    Error
            end;
        Error ->
            Error
    end.

remove(Name) ->
    %% Stop and uninstall
    stop_release(Name),
    uninstall_release(Name).

upgrade(#app_spec{name = Name, version = NewVersion, source = Source} = AppSpec, OldVersion) ->
    %% Download new release
    case download_release(Name, NewVersion, Source) of
        {ok, TarPath} ->
            %% Install alongside old version
            case install_release(Name, NewVersion, TarPath) of
                ok ->
                    %% Perform relup if available, otherwise restart
                    case has_relup(Name, OldVersion, NewVersion) of
                        true ->
                            upgrade_release(Name, NewVersion, AppSpec);
                        false ->
                            stop_release(Name),
                            start_release(Name, NewVersion, AppSpec)
                    end;
                Error ->
                    Error
            end;
        Error ->
            Error
    end.

reconfigure(AppSpec) ->
    %% Update sys.config and signal reload
    update_sys_config(AppSpec),
    reload_config(AppSpec#app_spec.name).

%% Implementation details (simplified)
download_release(Name, Version, #source_spec{type = release, url = Url, sha256 = Sha256}) ->
    TarPath = filename:join([?RELEASES_DIR, Name, Version ++ ".tar.gz"]),
    filelib:ensure_dir(TarPath),
    %% Download and verify
    case httpc:request(get, {binary_to_list(Url), []}, [], [{stream, TarPath}]) of
        {ok, saved_to_file} ->
            case verify_checksum(TarPath, Sha256) of
                ok -> {ok, TarPath};
                Error -> Error
            end;
        Error ->
            Error
    end;
download_release(_Name, _Version, _Source) ->
    {error, unsupported_source}.

install_release(_Name, _Version, _TarPath) ->
    %% Extract tarball to releases directory
    ok.

start_release(Name, Version, #app_spec{env = Env}) ->
    %% Start using release script
    RelDir = filename:join([?RELEASES_DIR, Name, Version]),
    Script = filename:join([RelDir, "bin", atom_to_list(Name)]),
    case os:cmd(Script ++ " start") of
        "ok\n" ->
            {ok, #app_state{
                name = Name,
                version = Version,
                status = running,
                path = RelDir,
                started_at = calendar:universal_time(),
                health = unknown,
                env = Env
            }};
        Error ->
            {error, {start_failed, Error}}
    end.

stop_release(Name) ->
    %% Stop using release script
    ok.

uninstall_release(_Name) ->
    ok.

has_relup(_Name, _OldVersion, _NewVersion) ->
    false.

upgrade_release(_Name, _Version, _AppSpec) ->
    ok.

update_sys_config(_AppSpec) ->
    ok.

reload_config(_Name) ->
    ok.

verify_checksum(_Path, undefined) ->
    ok;
verify_checksum(Path, ExpectedSha256) ->
    {ok, Data} = file:read_file(Path),
    Actual = crypto:hash(sha256, Data),
    case Actual =:= base64:decode(ExpectedSha256) of
        true -> ok;
        false -> {error, checksum_mismatch}
    end.
```

## Choosing a Strategy

| Scenario | Recommended Strategy |
|----------|---------------------|
| Development/Testing | Simple Start/Stop |
| Stateless services | Blue-Green |
| Stateful services | Hot Code Upgrade |
| Microservices with load balancer | Blue-Green |
| Monolithic applications | Hot Code Upgrade or Release-Based |
| Edge/IoT devices | Release-Based |

## Health Checks

Implement health checks to verify deployments:

```erlang
check_health(#app_spec{name = Name, health = undefined}) ->
    %% No health check configured - assume healthy if running
    case lists:keyfind(Name, 1, application:which_applications()) of
        {Name, _, _} -> healthy;
        false -> unhealthy
    end;

check_health(#app_spec{name = Name, health = #health_spec{type = http, port = Port, path = Path, timeout = Timeout}}) ->
    Url = "http://localhost:" ++ integer_to_list(Port) ++ binary_to_list(Path),
    case httpc:request(get, {Url, []}, [{timeout, Timeout}], []) of
        {ok, {{_, 200, _}, _, _}} -> healthy;
        _ -> unhealthy
    end;

check_health(#app_spec{health = #health_spec{type = tcp, port = Port, timeout = Timeout}}) ->
    case gen_tcp:connect("localhost", Port, [], Timeout) of
        {ok, Socket} ->
            gen_tcp:close(Socket),
            healthy;
        _ ->
            unhealthy
    end;

check_health(#app_spec{health = #health_spec{type = custom, module = Module}}) ->
    Module:check().
```

## State Tracking

Track application state for accurate reconciliation:

```erlang
-module(state_tracker).

-define(STATE_TABLE, bc_gitops_app_state).

init() ->
    ets:new(?STATE_TABLE, [named_table, public, {keypos, 2}]).

store(#app_state{} = State) ->
    ets:insert(?STATE_TABLE, State).

get(Name) ->
    case ets:lookup(?STATE_TABLE, Name) of
        [State] -> {ok, State};
        [] -> {error, not_found}
    end.

get_all() ->
    {ok, maps:from_list([{S#app_state.name, S} || S <- ets:tab2list(?STATE_TABLE)])}.

remove(Name) ->
    ets:delete(?STATE_TABLE, Name).
```

## Error Handling and Rollback

Always plan for failures:

```erlang
safe_upgrade(AppSpec, OldVersion, Runtime) ->
    %% Take snapshot before upgrade
    {ok, OldState} = Runtime:get_current_state(),

    case Runtime:upgrade(AppSpec, OldVersion) of
        {ok, NewState} ->
            %% Verify health after upgrade
            case check_health(AppSpec) of
                healthy ->
                    {ok, NewState};
                unhealthy ->
                    %% Rollback
                    logger:error("Health check failed after upgrade, rolling back"),
                    OldSpec = AppSpec#app_spec{version = OldVersion},
                    Runtime:upgrade(OldSpec, AppSpec#app_spec.version)
            end;
        {error, Reason} ->
            logger:error("Upgrade failed: ~p", [Reason]),
            {error, Reason}
    end.
```

## Testing Your Runtime

```erlang
-module(my_runtime_tests).
-include_lib("eunit/include/eunit.hrl").
-include_lib("bc_gitops/include/bc_gitops.hrl").

deploy_and_remove_test() ->
    Spec = #app_spec{
        name = test_app,
        version = <<"1.0.0">>,
        source = #source_spec{type = hex},
        env = #{},
        depends_on = []
    },

    %% Deploy
    {ok, State} = my_runtime:deploy(Spec),
    ?assertEqual(running, State#app_state.status),

    %% Remove
    ok = my_runtime:remove(test_app).

upgrade_preserves_state_test() ->
    %% Test that upgrades don't lose application state
    ok.
```
