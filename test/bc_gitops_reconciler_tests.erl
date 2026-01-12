%%% @doc Unit tests for bc_gitops_reconciler module.
%%%
%%% Uses meck to mock git operations and runtime callbacks.
-module(bc_gitops_reconciler_tests).

-include_lib("eunit/include/eunit.hrl").
-include("bc_gitops.hrl").

%% -----------------------------------------------------------------------------
%% Test fixtures
%% -----------------------------------------------------------------------------

test_config() ->
    #{
        repo_url => <<"https://github.com/test/repo.git">>,
        local_path => <<"/tmp/bc_gitops_test">>,
        branch => <<"main">>,
        apps_dir => <<"apps">>,
        reconcile_interval => 60000,
        runtime_module => test_runtime
    }.

app_spec(Name, Version) ->
    app_spec(Name, Version, []).

app_spec(Name, Version, DependsOn) ->
    #app_spec{
        name = Name,
        version = Version,
        source = #source_spec{type = hex},
        env = #{},
        health = undefined,
        depends_on = DependsOn
    }.

app_state(Name, Version) ->
    #app_state{
        name = Name,
        version = Version,
        status = running,
        path = undefined,
        pid = undefined,
        started_at = calendar:universal_time(),
        health = healthy,
        env = #{}
    }.

%% -----------------------------------------------------------------------------
%% Dependency sorting tests
%% -----------------------------------------------------------------------------

sort_deploys_no_deps_test() ->
    %% Apps with no dependencies - order doesn't matter
    DesiredState = #{
        app_a => app_spec(app_a, <<"1.0.0">>),
        app_b => app_spec(app_b, <<"1.0.0">>)
    },
    Actions = [
        {deploy, app_spec(app_a, <<"1.0.0">>)},
        {deploy, app_spec(app_b, <<"1.0.0">>)}
    ],

    Sorted = bc_gitops_reconciler:sort_by_dependencies(Actions, DesiredState),

    ?assertEqual(2, length(Sorted)),
    %% Both should be deploy actions
    ?assert(lists:all(fun({deploy, _}) -> true; (_) -> false end, Sorted)).

sort_deploys_with_deps_test() ->
    %% app_b depends on app_a, so app_a should be deployed first
    DesiredState = #{
        app_a => app_spec(app_a, <<"1.0.0">>, []),
        app_b => app_spec(app_b, <<"1.0.0">>, [app_a])
    },
    Actions = [
        {deploy, app_spec(app_b, <<"1.0.0">>, [app_a])},
        {deploy, app_spec(app_a, <<"1.0.0">>, [])}
    ],

    Sorted = bc_gitops_reconciler:sort_by_dependencies(Actions, DesiredState),

    ?assertEqual(2, length(Sorted)),
    %% app_a should come before app_b
    [First, Second] = Sorted,
    {deploy, #app_spec{name = FirstName}} = First,
    {deploy, #app_spec{name = SecondName}} = Second,
    ?assertEqual(app_a, FirstName),
    ?assertEqual(app_b, SecondName).

sort_deploys_chain_deps_test() ->
    %% app_c -> app_b -> app_a (chain dependency)
    DesiredState = #{
        app_a => app_spec(app_a, <<"1.0.0">>, []),
        app_b => app_spec(app_b, <<"1.0.0">>, [app_a]),
        app_c => app_spec(app_c, <<"1.0.0">>, [app_b])
    },
    Actions = [
        {deploy, app_spec(app_c, <<"1.0.0">>, [app_b])},
        {deploy, app_spec(app_a, <<"1.0.0">>, [])},
        {deploy, app_spec(app_b, <<"1.0.0">>, [app_a])}
    ],

    Sorted = bc_gitops_reconciler:sort_by_dependencies(Actions, DesiredState),

    Names = [N || {deploy, #app_spec{name = N}} <- Sorted],
    ?assertEqual([app_a, app_b, app_c], Names).

sort_removes_with_deps_test() ->
    %% When removing, dependents should be removed first
    %% app_b depends on app_a, so app_b should be removed first
    DesiredState = #{
        app_a => app_spec(app_a, <<"1.0.0">>, []),
        app_b => app_spec(app_b, <<"1.0.0">>, [app_a])
    },
    Actions = [
        {remove, app_state(app_a, <<"1.0.0">>)},
        {remove, app_state(app_b, <<"1.0.0">>)}
    ],

    Sorted = bc_gitops_reconciler:sort_by_dependencies(Actions, DesiredState),

    Names = [N || {remove, #app_state{name = N}} <- Sorted],
    %% app_b (dependent) should be removed before app_a (dependency)
    ?assertEqual([app_b, app_a], Names).

sort_mixed_actions_test() ->
    %% Mix of removes, deploys, and upgrades
    DesiredState = #{
        app_a => app_spec(app_a, <<"1.0.0">>, []),
        app_b => app_spec(app_b, <<"1.0.0">>, [app_a]),
        app_c => app_spec(app_c, <<"2.0.0">>, [])
    },
    Actions = [
        {deploy, app_spec(app_b, <<"1.0.0">>, [app_a])},
        {remove, app_state(app_x, <<"1.0.0">>)},
        {upgrade, app_spec(app_c, <<"2.0.0">>, []), <<"1.0.0">>},
        {deploy, app_spec(app_a, <<"1.0.0">>, [])}
    ],

    Sorted = bc_gitops_reconciler:sort_by_dependencies(Actions, DesiredState),

    %% Should be: removes first, then deploys (sorted), then upgrades
    ?assertEqual(4, length(Sorted)),

    %% First should be remove
    [{remove, _} | Rest1] = Sorted,

    %% Next two should be deploys in dependency order
    [{deploy, #app_spec{name = app_a}}, {deploy, #app_spec{name = app_b}} | Rest2] = Rest1,

    %% Last should be upgrade
    [{upgrade, _, _}] = Rest2.

%% -----------------------------------------------------------------------------
%% Calculate actions tests
%% -----------------------------------------------------------------------------

calculate_actions_deploy_new_test() ->
    %% New app in desired state should result in deploy action
    DesiredState = #{
        app_a => app_spec(app_a, <<"1.0.0">>)
    },
    CurrentState = #{},

    Actions = bc_gitops_reconciler:calculate_actions(DesiredState, CurrentState),

    ?assertEqual(1, length(Actions)),
    [{deploy, Spec}] = Actions,
    ?assertEqual(app_a, Spec#app_spec.name).

calculate_actions_remove_old_test() ->
    %% App in current but not desired should result in remove action
    DesiredState = #{},
    CurrentState = #{
        app_a => app_state(app_a, <<"1.0.0">>)
    },

    Actions = bc_gitops_reconciler:calculate_actions(DesiredState, CurrentState),

    ?assertEqual(1, length(Actions)),
    [{remove, State}] = Actions,
    ?assertEqual(app_a, State#app_state.name).

calculate_actions_upgrade_test() ->
    %% Same app, different version should result in upgrade action
    DesiredState = #{
        app_a => app_spec(app_a, <<"2.0.0">>)
    },
    CurrentState = #{
        app_a => app_state(app_a, <<"1.0.0">>)
    },

    Actions = bc_gitops_reconciler:calculate_actions(DesiredState, CurrentState),

    ?assertEqual(1, length(Actions)),
    [{upgrade, Spec, OldVersion}] = Actions,
    ?assertEqual(app_a, Spec#app_spec.name),
    ?assertEqual(<<"2.0.0">>, Spec#app_spec.version),
    ?assertEqual(<<"1.0.0">>, OldVersion).

calculate_actions_reconfigure_test() ->
    %% Same version but different env should result in reconfigure action
    DesiredSpec = #app_spec{
        name = app_a,
        version = <<"1.0.0">>,
        source = #source_spec{type = hex},
        env = #{port => 9090},
        health = undefined,
        depends_on = []
    },
    CurrentState = #app_state{
        name = app_a,
        version = <<"1.0.0">>,
        status = running,
        path = undefined,
        pid = undefined,
        started_at = calendar:universal_time(),
        health = healthy,
        env = #{port => 8080}
    },

    DesiredState = #{app_a => DesiredSpec},
    CurrentStateMap = #{app_a => CurrentState},

    Actions = bc_gitops_reconciler:calculate_actions(DesiredState, CurrentStateMap),

    ?assertEqual(1, length(Actions)),
    [{reconfigure, Spec}] = Actions,
    ?assertEqual(app_a, Spec#app_spec.name).

calculate_actions_no_change_test() ->
    %% Same version and env should result in no actions
    Spec = app_spec(app_a, <<"1.0.0">>),
    State = app_state(app_a, <<"1.0.0">>),

    DesiredState = #{app_a => Spec},
    CurrentState = #{app_a => State},

    Actions = bc_gitops_reconciler:calculate_actions(DesiredState, CurrentState),

    ?assertEqual([], Actions).

%% -----------------------------------------------------------------------------
%% Integration tests with mocked git/runtime
%% -----------------------------------------------------------------------------

reconciler_start_stop_test_() ->
    {setup,
        fun setup_mocks/0,
        fun cleanup_mocks/1,
        fun(_) ->
            [
                ?_test(reconciler_starts_and_stops())
            ]
        end
    }.

setup_mocks() ->
    meck:new(bc_gitops_git, [passthrough]),
    meck:new(bc_gitops_parser, [passthrough]),
    meck:new(test_runtime, [non_strict]),

    %% Mock git operations
    meck:expect(bc_gitops_git, ensure_repo, fun(_, _, _) -> ok end),
    meck:expect(bc_gitops_git, pull, fun(_, _) -> {ok, <<"abc123">>} end),

    %% Mock parser
    meck:expect(bc_gitops_parser, parse_apps_dir, fun(_) -> {ok, #{}} end),

    %% Mock runtime
    meck:expect(test_runtime, get_current_state, fun() -> {ok, #{}} end),
    meck:expect(test_runtime, deploy, fun(#app_spec{name = N, version = V}) ->
        {ok, app_state(N, V)}
    end),
    meck:expect(test_runtime, remove, fun(_) -> ok end),

    ok.

cleanup_mocks(_) ->
    meck:unload(bc_gitops_git),
    meck:unload(bc_gitops_parser),
    meck:unload(test_runtime),
    ok.

reconciler_starts_and_stops() ->
    Config = test_config(),
    {ok, Pid} = bc_gitops_reconciler:start_link(Config),
    ?assert(is_process_alive(Pid)),

    %% Give it time to initialize
    timer:sleep(100),

    %% Check status
    {ok, Status} = bc_gitops_reconciler:status(),
    ?assert(is_map(Status)),

    %% Stop
    gen_server:stop(Pid),
    timer:sleep(50),
    ?assertNot(is_process_alive(Pid)).
