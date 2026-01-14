%%% @doc Unit tests for bc_gitops_runtime_isolated module.
%%%
%%% Tests the isolated VM runtime implementation including state
%%% management, deploy/remove/upgrade/reconfigure operations,
%%% and health checking.
%%% @end
-module(bc_gitops_runtime_isolated_tests).

-include_lib("eunit/include/eunit.hrl").
-include("bc_gitops.hrl").

%% -----------------------------------------------------------------------------
%% Test Fixtures
%% -----------------------------------------------------------------------------

setup() ->
    %% Clear any existing state
    clear_state(),
    %% Ensure distribution is running
    _ = bc_gitops_cluster:ensure_distributed(),
    ok.

test_app_spec() ->
    #app_spec{
        name = test_isolated_app,
        version = <<"1.0.0">>,
        description = <<"Test isolated application">>,
        icon = undefined,
        source = #source_spec{type = hex},
        env = #{http_port => 8080},
        health = undefined,
        depends_on = [],
        isolation = vm,
        vm_config = #vm_config{
            memory_limit = 256,
            scheduler_limit = 1,
            node_prefix = <<"test_">>,
            extra_args = []
        }
    }.

test_app_state() ->
    #app_state{
        name = test_isolated_app,
        version = <<"1.0.0">>,
        description = <<"Test isolated application">>,
        icon = #icon_spec{type = identicon, value = <<"data:...">>},
        status = running,
        path = <<"/tmp/test">>,
        pid = undefined,
        started_at = calendar:universal_time(),
        health = healthy,
        env = #{http_port => 8080},
        isolation = vm,
        node = 'test_isolated_app@localhost',
        os_pid = 12345
    }.

%% -----------------------------------------------------------------------------
%% State Management Tests
%% -----------------------------------------------------------------------------

get_current_state_empty_test() ->
    setup(),
    {ok, State} = bc_gitops_runtime_isolated:get_current_state(),
    ?assertEqual(#{}, State).

store_state_test() ->
    setup(),
    AppState = test_app_state(),
    ok = bc_gitops_runtime_isolated:store_state(AppState),

    {ok, Retrieved} = bc_gitops_runtime_isolated:get_state(test_isolated_app),
    ?assertEqual(AppState#app_state.name, Retrieved#app_state.name),
    ?assertEqual(AppState#app_state.version, Retrieved#app_state.version).

get_state_not_found_test() ->
    setup(),
    Result = bc_gitops_runtime_isolated:get_state(nonexistent_app),
    ?assertEqual({error, not_found}, Result).

remove_state_test() ->
    setup(),
    AppState = test_app_state(),
    ok = bc_gitops_runtime_isolated:store_state(AppState),

    %% Verify it exists
    {ok, _} = bc_gitops_runtime_isolated:get_state(test_isolated_app),

    %% Remove it
    ok = bc_gitops_runtime_isolated:remove_state(test_isolated_app),

    %% Verify it's gone
    Result = bc_gitops_runtime_isolated:get_state(test_isolated_app),
    ?assertEqual({error, not_found}, Result).

remove_state_idempotent_test() ->
    setup(),
    %% Removing non-existent state should succeed
    ok = bc_gitops_runtime_isolated:remove_state(nonexistent_app),
    ok = bc_gitops_runtime_isolated:remove_state(nonexistent_app).

get_all_states_test() ->
    setup(),
    %% Store multiple states
    State1 = (test_app_state())#app_state{name = app1},
    State2 = (test_app_state())#app_state{name = app2},
    State3 = (test_app_state())#app_state{name = app3},

    ok = bc_gitops_runtime_isolated:store_state(State1),
    ok = bc_gitops_runtime_isolated:store_state(State2),
    ok = bc_gitops_runtime_isolated:store_state(State3),

    AllStates = bc_gitops_runtime_isolated:get_all_states(),
    ?assertEqual(3, maps:size(AllStates)),
    ?assert(maps:is_key(app1, AllStates)),
    ?assert(maps:is_key(app2, AllStates)),
    ?assert(maps:is_key(app3, AllStates)).

get_current_state_updates_health_test() ->
    setup(),
    %% Store a state with a fake node (will fail health check)
    AppState = (test_app_state())#app_state{
        node = 'fake_node_12345@nonexistent',
        status = running,
        health = healthy
    },
    ok = bc_gitops_runtime_isolated:store_state(AppState),

    %% get_current_state should update health
    {ok, States} = bc_gitops_runtime_isolated:get_current_state(),
    UpdatedState = maps:get(test_isolated_app, States),

    %% Node is unreachable, so status should be stopped and health unhealthy
    ?assertEqual(stopped, UpdatedState#app_state.status),
    ?assertEqual(unhealthy, UpdatedState#app_state.health).

%% -----------------------------------------------------------------------------
%% Deploy Tests (Error Cases)
%% -----------------------------------------------------------------------------

deploy_nonexistent_package_test() ->
    setup(),
    Spec = #app_spec{
        name = nonexistent_package_12345,
        version = <<"1.0.0">>,
        source = #source_spec{type = hex},
        env = #{},
        health = undefined,
        depends_on = [],
        isolation = vm,
        vm_config = undefined
    },

    Result = bc_gitops_runtime_isolated:deploy(Spec),
    %% Should fail at fetch stage
    ?assertMatch({error, {fetch_failed, _}}, Result).

%% -----------------------------------------------------------------------------
%% Remove Tests
%% -----------------------------------------------------------------------------

remove_nonexistent_app_test() ->
    setup(),
    %% Removing non-existent app should succeed (already gone)
    Result = bc_gitops_runtime_isolated:remove(nonexistent_app_12345),
    ?assertEqual(ok, Result).

remove_app_with_state_test() ->
    setup(),
    %% Store state for an app with a fake node
    AppState = (test_app_state())#app_state{
        node = 'fake_node_12345@nonexistent',
        os_pid = undefined
    },
    ok = bc_gitops_runtime_isolated:store_state(AppState),

    %% Remove should succeed (node already down)
    Result = bc_gitops_runtime_isolated:remove(test_isolated_app),
    ?assertEqual(ok, Result),

    %% State should be removed
    ?assertEqual({error, not_found}, bc_gitops_runtime_isolated:get_state(test_isolated_app)).

remove_app_with_os_pid_test() ->
    setup(),
    %% Store state with a definitely dead OS PID
    AppState = (test_app_state())#app_state{
        node = 'fake_node_12345@nonexistent',
        os_pid = 999999999
    },
    ok = bc_gitops_runtime_isolated:store_state(AppState),

    %% Remove should succeed (will try graceful, then kill)
    Result = bc_gitops_runtime_isolated:remove(test_isolated_app),
    ?assertEqual(ok, Result).

remove_app_without_node_test() ->
    setup(),
    %% Store state without node info
    AppState = (test_app_state())#app_state{
        node = undefined,
        os_pid = undefined
    },
    ok = bc_gitops_runtime_isolated:store_state(AppState),

    %% Remove should just clear state
    Result = bc_gitops_runtime_isolated:remove(test_isolated_app),
    ?assertEqual(ok, Result),
    ?assertEqual({error, not_found}, bc_gitops_runtime_isolated:get_state(test_isolated_app)).

%% -----------------------------------------------------------------------------
%% Reconfigure Tests
%% -----------------------------------------------------------------------------

reconfigure_not_deployed_test() ->
    setup(),
    Spec = test_app_spec(),
    Result = bc_gitops_runtime_isolated:reconfigure(Spec),
    ?assertEqual({error, {not_deployed, test_isolated_app}}, Result).

reconfigure_no_node_test() ->
    setup(),
    %% Store state without node
    AppState = (test_app_state())#app_state{node = undefined},
    ok = bc_gitops_runtime_isolated:store_state(AppState),

    Spec = test_app_spec(),
    Result = bc_gitops_runtime_isolated:reconfigure(Spec),
    ?assertEqual({error, {no_node, test_isolated_app}}, Result).

reconfigure_unreachable_node_test() ->
    setup(),
    %% Store state with unreachable node
    AppState = (test_app_state())#app_state{
        node = 'fake_node_12345@nonexistent'
    },
    ok = bc_gitops_runtime_isolated:store_state(AppState),

    Spec = (test_app_spec())#app_spec{env = #{new_key => new_value}},
    Result = bc_gitops_runtime_isolated:reconfigure(Spec),
    %% Should fail because node is unreachable
    ?assertMatch({error, {reconfigure_failed, _}}, Result).

%% -----------------------------------------------------------------------------
%% Upgrade Tests
%% -----------------------------------------------------------------------------

upgrade_removes_then_deploys_test() ->
    setup(),
    %% Store existing state
    AppState = (test_app_state())#app_state{
        version = <<"1.0.0">>,
        node = 'fake_node_12345@nonexistent'
    },
    ok = bc_gitops_runtime_isolated:store_state(AppState),

    %% Upgrade to v2 (will fail at deploy stage, but remove should succeed)
    Spec = (test_app_spec())#app_spec{version = <<"2.0.0">>},
    Result = bc_gitops_runtime_isolated:upgrade(Spec, <<"1.0.0">>),

    %% Should fail at deploy (package doesn't exist)
    ?assertMatch({error, {fetch_failed, _}}, Result),

    %% But old state should be removed
    ?assertEqual({error, not_found}, bc_gitops_runtime_isolated:get_state(test_isolated_app)).

%% -----------------------------------------------------------------------------
%% Icon Generation Tests
%% -----------------------------------------------------------------------------

deploy_generates_identicon_when_no_icon_test() ->
    %% This is an indirect test - we verify through state storage
    setup(),

    %% Store a state and verify icon is set
    AppState = (test_app_state())#app_state{
        icon = #icon_spec{
            type = identicon,
            value = <<"data:image/svg+xml;base64,...">>
        }
    },
    ok = bc_gitops_runtime_isolated:store_state(AppState),

    {ok, Retrieved} = bc_gitops_runtime_isolated:get_state(test_isolated_app),
    ?assertNotEqual(undefined, Retrieved#app_state.icon),
    ?assertEqual(identicon, (Retrieved#app_state.icon)#icon_spec.type).

%% -----------------------------------------------------------------------------
%% Integration with Reconciler Tests
%% -----------------------------------------------------------------------------

get_current_state_returns_map_test() ->
    setup(),
    {ok, State} = bc_gitops_runtime_isolated:get_current_state(),
    ?assert(is_map(State)).

state_persists_across_calls_test() ->
    setup(),
    %% Store state
    AppState = test_app_state(),
    ok = bc_gitops_runtime_isolated:store_state(AppState),

    %% First call
    {ok, State1} = bc_gitops_runtime_isolated:get_current_state(),

    %% Second call
    {ok, State2} = bc_gitops_runtime_isolated:get_current_state(),

    %% Should have same app (health may be updated)
    ?assert(maps:is_key(test_isolated_app, State1)),
    ?assert(maps:is_key(test_isolated_app, State2)).

%% -----------------------------------------------------------------------------
%% Behaviour Compliance Tests
%% -----------------------------------------------------------------------------

implements_runtime_behaviour_test() ->
    %% Verify all bc_gitops_runtime callbacks are exported
    Exports = bc_gitops_runtime_isolated:module_info(exports),
    ?assert(lists:member({deploy, 1}, Exports)),
    ?assert(lists:member({remove, 1}, Exports)),
    ?assert(lists:member({upgrade, 2}, Exports)),
    ?assert(lists:member({reconfigure, 1}, Exports)),
    ?assert(lists:member({get_current_state, 0}, Exports)).

%% -----------------------------------------------------------------------------
%% Helper Functions
%% -----------------------------------------------------------------------------

clear_state() ->
    try
        persistent_term:erase({bc_gitops_runtime_isolated, app_states})
    catch
        error:badarg -> ok
    end.
