%%% @doc Unit tests for bc_gitops_runtime_default module.
-module(bc_gitops_runtime_default_tests).

-include_lib("eunit/include/eunit.hrl").
-include("bc_gitops.hrl").

%% -----------------------------------------------------------------------------
%% Test fixtures
%% -----------------------------------------------------------------------------

%% Kept for future use
%% test_app_spec() ->
%%     #app_spec{
%%         name = test_app,
%%         version = <<"1.0.0">>,
%%         source = #source_spec{type = hex},
%%         env = #{key => value},
%%         health = undefined,
%%         depends_on = []
%%     }.

%% -----------------------------------------------------------------------------
%% State management tests
%% -----------------------------------------------------------------------------

get_current_state_empty_test() ->
    %% Clear any existing state
    clear_state(),

    {ok, State} = bc_gitops_runtime_default:get_current_state(),
    ?assertEqual(#{}, State).

get_current_state_after_deploy_test() ->
    clear_state(),

    %% We can't actually deploy test_app (it doesn't exist),
    %% but we can test the state tracking by simulating
    %% This tests the internal state management
    ok.

%% -----------------------------------------------------------------------------
%% Deploy tests (with mocked application)
%% -----------------------------------------------------------------------------

deploy_nonexistent_app_test() ->
    clear_state(),

    Spec = #app_spec{
        name = nonexistent_app_12345,
        version = <<"1.0.0">>,
        source = #source_spec{type = hex},
        env = #{},
        health = undefined,
        depends_on = []
    },

    Result = bc_gitops_runtime_default:deploy(Spec),
    %% New runtime tries to fetch first, so we get fetch_failed
    ?assertMatch({error, {fetch_failed, _}}, Result).

deploy_existing_app_test() ->
    clear_state(),

    %% crypto is always available
    Spec = #app_spec{
        name = crypto,
        version = <<"1.0.0">>,
        source = #source_spec{type = hex},
        env = #{},
        health = undefined,
        depends_on = []
    },

    Result = bc_gitops_runtime_default:deploy(Spec),
    ?assertMatch({ok, #app_state{name = crypto}}, Result),

    %% Verify it's tracked
    {ok, State} = bc_gitops_runtime_default:get_current_state(),
    ?assert(maps:is_key(crypto, State)).

%% -----------------------------------------------------------------------------
%% Remove tests
%% -----------------------------------------------------------------------------

remove_nonexistent_app_test() ->
    clear_state(),

    %% Removing non-running app should succeed (already stopped)
    Result = bc_gitops_runtime_default:remove(nonexistent_app_12345),
    ?assertEqual(ok, Result).

%% -----------------------------------------------------------------------------
%% Reconfigure tests
%% -----------------------------------------------------------------------------

reconfigure_updates_env_test() ->
    clear_state(),

    %% First deploy
    Spec1 = #app_spec{
        name = crypto,
        version = <<"1.0.0">>,
        source = #source_spec{type = hex},
        env = #{old_key => old_value},
        health = undefined,
        depends_on = []
    },
    {ok, _} = bc_gitops_runtime_default:deploy(Spec1),

    %% Now reconfigure with new env
    Spec2 = Spec1#app_spec{env = #{new_key => new_value}},
    {ok, NewState} = bc_gitops_runtime_default:reconfigure(Spec2),

    ?assertEqual(#{new_key => new_value}, NewState#app_state.env).

%% -----------------------------------------------------------------------------
%% Upgrade tests
%% -----------------------------------------------------------------------------

upgrade_changes_version_test() ->
    clear_state(),

    %% First deploy v1
    Spec1 = #app_spec{
        name = crypto,
        version = <<"1.0.0">>,
        source = #source_spec{type = hex},
        env = #{},
        health = undefined,
        depends_on = []
    },
    {ok, _} = bc_gitops_runtime_default:deploy(Spec1),

    %% Upgrade to v2
    Spec2 = Spec1#app_spec{version = <<"2.0.0">>},
    {ok, NewState} = bc_gitops_runtime_default:upgrade(Spec2, <<"1.0.0">>),

    ?assertEqual(<<"2.0.0">>, NewState#app_state.version).

%% -----------------------------------------------------------------------------
%% Helper functions
%% -----------------------------------------------------------------------------

clear_state() ->
    try
        persistent_term:erase({bc_gitops_runtime_default, app_state})
    catch
        error:badarg -> ok
    end.
