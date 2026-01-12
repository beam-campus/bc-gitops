%%% @doc Unit tests for bc_gitops_hot_reload module.
-module(bc_gitops_hot_reload_tests).

-include_lib("eunit/include/eunit.hrl").

%% -----------------------------------------------------------------------------
%% Tests for get_app_modules
%% -----------------------------------------------------------------------------

get_app_modules_kernel_test() ->
    %% kernel is always loaded
    {ok, Modules} = bc_gitops_hot_reload:get_app_modules(kernel),
    ?assert(is_list(Modules)),
    ?assert(length(Modules) > 0),
    ?assert(lists:member(application, Modules)).

get_app_modules_nonexistent_test() ->
    %% Non-existent app should fail
    Result = bc_gitops_hot_reload:get_app_modules(nonexistent_app_xyz_12345),
    ?assertMatch({error, _}, Result).

%% -----------------------------------------------------------------------------
%% Tests for get_module_beam_hash
%% -----------------------------------------------------------------------------

get_module_beam_hash_existing_test() ->
    %% Get hash for a known module
    {ok, Hash} = bc_gitops_hot_reload:get_module_beam_hash(lists),
    ?assert(is_binary(Hash)),
    ?assertEqual(16, byte_size(Hash)).  %% MD5 is 16 bytes

get_module_beam_hash_nonexistent_test() ->
    Result = bc_gitops_hot_reload:get_module_beam_hash(nonexistent_module_xyz),
    ?assertMatch({error, {module_not_found, _}}, Result).

%% -----------------------------------------------------------------------------
%% Tests for reload_module
%% -----------------------------------------------------------------------------

reload_module_test() ->
    %% Test reloading a module that's already loaded
    %% Using bc_gitops_hot_reload itself as the test subject
    Result = bc_gitops_hot_reload:reload_module(bc_gitops_hot_reload),
    ?assertMatch({ok, bc_gitops_hot_reload}, Result).

reload_modules_test() ->
    %% Test reloading multiple modules
    Result = bc_gitops_hot_reload:reload_modules([bc_gitops_hot_reload, bc_gitops_workspace]),
    ?assertMatch({ok, _}, Result).

%% -----------------------------------------------------------------------------
%% Tests for reload_changed_modules
%% -----------------------------------------------------------------------------

reload_changed_modules_no_changes_test() ->
    %% When modules haven't changed, should return empty list
    {ok, Changed} = bc_gitops_hot_reload:reload_changed_modules(bc_gitops),
    ?assertEqual([], Changed).  %% No changes since we just compiled

%% -----------------------------------------------------------------------------
%% Tests for upgrade_app (basic)
%% -----------------------------------------------------------------------------

upgrade_app_stdlib_test() ->
    %% Test upgrade on stdlib (won't actually change anything, but tests the flow)
    Result = bc_gitops_hot_reload:upgrade_app(stdlib, <<"1.0.0">>, <<"2.0.0">>),
    ?assertEqual(ok, Result).
