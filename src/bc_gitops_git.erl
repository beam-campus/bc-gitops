%%% @doc Git operations for bc_gitops.
%%%
%%% This module wraps git commands for cloning, pulling, and
%%% querying repository state. It uses the system git command
%%% for maximum compatibility.
%%%
%%% Git authentication is handled through standard git mechanisms:
%%% SSH keys, credential helpers, or inline URL credentials.
%%% For CI/CD environments, SSH deploy keys are recommended.
%%% @end
-module(bc_gitops_git).

-include("bc_gitops.hrl").

%% API
-export([
    ensure_repo/3,
    pull/2,
    clone/3,
    get_head_commit/1,
    get_commit_log/2,
    checkout/2,
    is_repo/1
]).

%% -----------------------------------------------------------------------------
%% API
%% -----------------------------------------------------------------------------

%% @doc Ensure the repository exists locally, cloning if necessary.
%%
%% If the local path exists and is a git repository, this does nothing.
%% Otherwise, it clones the repository.
-spec ensure_repo(binary(), binary(), binary()) -> ok | {error, term()}.
ensure_repo(RepoUrl, LocalPath, Branch) ->
    LocalPathStr = binary_to_list(LocalPath),
    case is_repo(LocalPathStr) of
        true ->
            ok;
        false ->
            clone(RepoUrl, LocalPath, Branch)
    end.

%% @doc Pull the latest changes from the remote repository.
%%
%% Returns the commit SHA after pulling.
-spec pull(binary(), binary()) -> {ok, binary()} | {error, term()}.
pull(LocalPath, Branch) ->
    LocalPathStr = binary_to_list(LocalPath),
    BranchStr = binary_to_list(Branch),

    %% Fetch first
    case git_cmd(LocalPathStr, ["fetch", "origin", BranchStr]) of
        {ok, _} ->
            %% Reset to origin/branch (handles force pushes)
            case git_cmd(LocalPathStr, ["reset", "--hard", "origin/" ++ BranchStr]) of
                {ok, _} ->
                    emit_telemetry(?TELEMETRY_GIT_PULL, #{}, #{
                        repo => LocalPath,
                        branch => Branch
                    }),
                    get_head_commit(LocalPath);
                {error, Reason} ->
                    {error, {reset_failed, Reason}}
            end;
        {error, Reason} ->
            {error, {fetch_failed, Reason}}
    end.

%% @doc Clone a repository to the local path.
-spec clone(binary(), binary(), binary()) -> ok | {error, term()}.
clone(RepoUrl, LocalPath, Branch) ->
    LocalPathStr = binary_to_list(LocalPath),
    RepoUrlStr = binary_to_list(RepoUrl),
    BranchStr = binary_to_list(Branch),

    %% Ensure parent directory exists
    ParentDir = filename:dirname(LocalPathStr),
    ok = filelib:ensure_dir(LocalPathStr ++ "/"),

    %% Clone with specific branch
    Args = ["clone", "--branch", BranchStr, "--single-branch", RepoUrlStr, LocalPathStr],
    case git_cmd(ParentDir, Args) of
        {ok, _} ->
            ok;
        {error, Reason} ->
            {error, {clone_failed, Reason}}
    end.

%% @doc Get the current HEAD commit SHA.
-spec get_head_commit(binary()) -> {ok, binary()} | {error, term()}.
get_head_commit(LocalPath) ->
    LocalPathStr = binary_to_list(LocalPath),
    case git_cmd(LocalPathStr, ["rev-parse", "HEAD"]) of
        {ok, Output} ->
            %% Trim newline
            Commit = string:trim(Output),
            {ok, list_to_binary(Commit)};
        {error, Reason} ->
            {error, Reason}
    end.

%% @doc Get the commit log for the last N commits.
%%
%% Returns a list of maps with commit info.
-spec get_commit_log(binary(), pos_integer()) -> {ok, [map()]} | {error, term()}.
get_commit_log(LocalPath, Count) ->
    LocalPathStr = binary_to_list(LocalPath),
    Format = "--format=%H|%an|%ae|%at|%s",
    Args = ["log", Format, "-n", integer_to_list(Count)],
    case git_cmd(LocalPathStr, Args) of
        {ok, Output} ->
            Lines = string:split(Output, "\n", all),
            Commits = lists:filtermap(
                fun(Line) ->
                    case string:split(Line, "|", all) of
                        [Hash, Author, Email, Timestamp, Subject] ->
                            {true, #{
                                hash => list_to_binary(Hash),
                                author => list_to_binary(Author),
                                email => list_to_binary(Email),
                                timestamp => list_to_integer(Timestamp),
                                subject => list_to_binary(Subject)
                            }};
                        _ ->
                            false
                    end
                end,
                Lines
            ),
            {ok, Commits};
        {error, Reason} ->
            {error, Reason}
    end.

%% @doc Checkout a specific ref (branch, tag, or commit).
-spec checkout(binary(), binary()) -> ok | {error, term()}.
checkout(LocalPath, Ref) ->
    LocalPathStr = binary_to_list(LocalPath),
    RefStr = binary_to_list(Ref),
    case git_cmd(LocalPathStr, ["checkout", RefStr]) of
        {ok, _} -> ok;
        {error, Reason} -> {error, {checkout_failed, Reason}}
    end.

%% @doc Check if a path is a git repository.
-spec is_repo(file:filename()) -> boolean().
is_repo(Path) ->
    GitDir = filename:join(Path, ".git"),
    filelib:is_dir(GitDir).

%% -----------------------------------------------------------------------------
%% Internal functions
%% -----------------------------------------------------------------------------

-spec git_cmd(file:filename(), [string()]) -> {ok, string()} | {error, term()}.
git_cmd(WorkDir, Args) ->
    Cmd = "git",
    Port = open_port(
        {spawn_executable, os:find_executable(Cmd)},
        [
            {args, Args},
            {cd, WorkDir},
            binary,
            exit_status,
            stderr_to_stdout,
            hide
        ]
    ),
    collect_port_output(Port, []).

-spec collect_port_output(port(), [binary()]) -> {ok, string()} | {error, term()}.
collect_port_output(Port, Acc) ->
    receive
        {Port, {data, Data}} ->
            collect_port_output(Port, [Data | Acc]);
        {Port, {exit_status, 0}} ->
            Output = lists:reverse(Acc),
            {ok, binary_to_list(iolist_to_binary(Output))};
        {Port, {exit_status, Code}} ->
            Output = lists:reverse(Acc),
            {error, {exit_code, Code, binary_to_list(iolist_to_binary(Output))}}
    after 60000 ->
        port_close(Port),
        {error, timeout}
    end.

%% -----------------------------------------------------------------------------
%% Telemetry
%% -----------------------------------------------------------------------------

-spec emit_telemetry([atom()], map(), map()) -> ok.
emit_telemetry(EventName, Measurements, Metadata) ->
    telemetry:execute(EventName, Measurements, Metadata).
