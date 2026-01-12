%%% @doc Unit tests for bc_gitops_git module.
-module(bc_gitops_git_tests).

-include_lib("eunit/include/eunit.hrl").

%% -----------------------------------------------------------------------------
%% is_repo/1 tests
%% -----------------------------------------------------------------------------

is_repo_with_git_dir_test() ->
    %% Create a fake git repo
    TmpDir = "/tmp/bc_gitops_git_test_" ++ integer_to_list(erlang:unique_integer([positive])),
    GitDir = filename:join(TmpDir, ".git"),

    ok = file:make_dir(TmpDir),
    ok = file:make_dir(GitDir),

    try
        ?assertEqual(true, bc_gitops_git:is_repo(TmpDir))
    after
        file:del_dir(GitDir),
        file:del_dir(TmpDir)
    end.

is_repo_without_git_dir_test() ->
    TmpDir = "/tmp/bc_gitops_git_test_" ++ integer_to_list(erlang:unique_integer([positive])),
    ok = file:make_dir(TmpDir),

    try
        ?assertEqual(false, bc_gitops_git:is_repo(TmpDir))
    after
        file:del_dir(TmpDir)
    end.

is_repo_nonexistent_dir_test() ->
    ?assertEqual(false, bc_gitops_git:is_repo("/nonexistent/path")).

%% -----------------------------------------------------------------------------
%% ensure_repo/3 tests
%% -----------------------------------------------------------------------------

ensure_repo_existing_repo_test() ->
    %% Create a fake git repo
    TmpDir = "/tmp/bc_gitops_git_test_" ++ integer_to_list(erlang:unique_integer([positive])),
    GitDir = filename:join(TmpDir, ".git"),

    ok = file:make_dir(TmpDir),
    ok = file:make_dir(GitDir),

    try
        %% Should return ok without cloning
        ?assertEqual(ok, bc_gitops_git:ensure_repo(
            <<"https://example.com/repo.git">>,
            list_to_binary(TmpDir),
            <<"main">>
        ))
    after
        file:del_dir(GitDir),
        file:del_dir(TmpDir)
    end.

%% Note: Testing actual clone/pull requires network access or mocking.
%% For full integration testing, use meck to mock os:cmd or run in
%% a CI environment with git access.
