%%% @doc Unit tests for bc_gitops_workspace module.
-module(bc_gitops_workspace_tests).

-include_lib("eunit/include/eunit.hrl").
-include("bc_gitops.hrl").

%% -----------------------------------------------------------------------------
%% Setup / Teardown
%% -----------------------------------------------------------------------------

setup() ->
    %% Use a test-specific workspace
    TestWorkspace = "/tmp/bc_gitops_test_workspace_" ++ integer_to_list(erlang:unique_integer([positive])),
    bc_gitops_workspace:init(TestWorkspace),
    TestWorkspace.

cleanup(TestWorkspace) ->
    bc_gitops_workspace:cleanup(),
    os:cmd("rm -rf " ++ TestWorkspace),
    ok.

%% -----------------------------------------------------------------------------
%% Test fixtures
%% -----------------------------------------------------------------------------

workspace_test_() ->
    {foreach,
     fun setup/0,
     fun cleanup/1,
     [
         fun init_creates_workspace/1,
         fun get_package_path_not_found/1,
         fun detect_project_type_rebar3/1,
         fun detect_project_type_mix/1,
         fun detect_project_type_erlang_mk/1,
         fun detect_project_type_unknown/1,
         fun run_cmd_success/1,
         fun run_cmd_failure/1
     ]}.

%% -----------------------------------------------------------------------------
%% Tests
%% -----------------------------------------------------------------------------

init_creates_workspace(TestWorkspace) ->
    fun() ->
        ?assert(filelib:is_dir(TestWorkspace))
    end.

get_package_path_not_found(_TestWorkspace) ->
    fun() ->
        Result = bc_gitops_workspace:get_package_path(nonexistent_package_xyz),
        ?assertEqual({error, not_found}, Result)
    end.

detect_project_type_rebar3(TestWorkspace) ->
    fun() ->
        ProjectDir = filename:join(TestWorkspace, "test_rebar3_project"),
        ok = filelib:ensure_dir(filename:join(ProjectDir, "dummy")),
        ok = file:write_file(filename:join(ProjectDir, "rebar.config"), "{deps, []}."),
        ?assertEqual(rebar3, bc_gitops_workspace:detect_project_type(ProjectDir))
    end.

detect_project_type_mix(TestWorkspace) ->
    fun() ->
        ProjectDir = filename:join(TestWorkspace, "test_mix_project"),
        ok = filelib:ensure_dir(filename:join(ProjectDir, "dummy")),
        ok = file:write_file(filename:join(ProjectDir, "mix.exs"), "defmodule Test.MixProject do end"),
        ?assertEqual(mix, bc_gitops_workspace:detect_project_type(ProjectDir))
    end.

detect_project_type_erlang_mk(TestWorkspace) ->
    fun() ->
        ProjectDir = filename:join(TestWorkspace, "test_erlang_mk_project"),
        ok = filelib:ensure_dir(filename:join(ProjectDir, "dummy")),
        ok = file:write_file(filename:join(ProjectDir, "Makefile"), "all:\n\techo ok"),
        ?assertEqual(erlang_mk, bc_gitops_workspace:detect_project_type(ProjectDir))
    end.

detect_project_type_unknown(TestWorkspace) ->
    fun() ->
        ProjectDir = filename:join(TestWorkspace, "test_unknown_project"),
        ok = filelib:ensure_dir(filename:join(ProjectDir, "dummy")),
        ?assertEqual(unknown, bc_gitops_workspace:detect_project_type(ProjectDir))
    end.

run_cmd_success(TestWorkspace) ->
    fun() ->
        {ok, Output} = bc_gitops_workspace:run_cmd(TestWorkspace, "echo hello"),
        ?assertEqual("hello\n", Output)
    end.

run_cmd_failure(TestWorkspace) ->
    fun() ->
        Result = bc_gitops_workspace:run_cmd(TestWorkspace, "exit 42"),
        ?assertMatch({error, {exit_code, 42, _}}, Result)
    end.

%% -----------------------------------------------------------------------------
%% Integration tests (require network)
%% -----------------------------------------------------------------------------

%% These tests actually fetch packages from hex.pm
%% They are slow and require network access, so they're in a separate group

integration_test_() ->
    {setup,
     fun() ->
         TestWorkspace = "/tmp/bc_gitops_integration_test_" ++ integer_to_list(erlang:unique_integer([positive])),
         bc_gitops_workspace:init(TestWorkspace),
         TestWorkspace
     end,
     fun(TestWorkspace) ->
         bc_gitops_workspace:cleanup(),
         os:cmd("rm -rf " ++ TestWorkspace),
         ok
     end,
     [
         %% Uncomment to run integration tests (slow, requires network)
         %% {"fetch_recon_from_hex", fun fetch_recon_test/0}
     ]}.

%% fetch_recon_test() ->
%%     %% Fetch a small, well-known Erlang package
%%     Source = #source_spec{type = hex, ref = <<"2.5.3">>},
%%     Result = bc_gitops_workspace:fetch_package(recon, Source),
%%     ?assertMatch({ok, _}, Result),
%%
%%     %% Verify the package is in the workspace
%%     {ok, PackagePath} = bc_gitops_workspace:get_package_path(recon),
%%     ?assert(filelib:is_dir(PackagePath)),
%%
%%     %% Verify ebin paths are available
%%     EbinPaths = bc_gitops_workspace:get_ebin_paths(recon),
%%     ?assert(length(EbinPaths) > 0).
