%%% @doc Workspace management for bc_gitops package operations.
%%%
%%% This module handles fetching, compiling, and managing packages
%%% in a dedicated workspace directory. It supports both Erlang (rebar3)
%%% and Elixir (mix) packages from hex.pm and git sources.
%%%
%%% @end
-module(bc_gitops_workspace).

-include("bc_gitops.hrl").

%% API
-export([
    init/0,
    init/1,
    fetch_package/2,
    remove_package/1,
    delete_package/1,
    get_package_path/1,
    get_ebin_paths/1,
    cleanup/0,
    purge_package_modules/1
]).

%% Internal exports for testing
-export([
    detect_project_type/1,
    run_cmd/2
]).

-define(DEFAULT_WORKSPACE, "/tmp/bc_gitops_workspace").
-define(WORKSPACE_KEY, {?MODULE, workspace_path}).

%% -----------------------------------------------------------------------------
%% API
%% -----------------------------------------------------------------------------

%% @doc Initialize the workspace with default path.
-spec init() -> ok | {error, term()}.
init() ->
    init(?DEFAULT_WORKSPACE).

%% @doc Initialize the workspace with a custom path.
-spec init(file:filename()) -> ok | {error, term()}.
init(WorkspacePath) ->
    case filelib:ensure_dir(filename:join(WorkspacePath, "dummy")) of
        ok ->
            persistent_term:put(?WORKSPACE_KEY, WorkspacePath),
            ok;
        {error, Reason} ->
            {error, {workspace_init_failed, Reason}}
    end.

%% @doc Fetch and compile a package.
%%
%% Returns the path to the compiled package directory.
-spec fetch_package(atom(), #source_spec{}) -> {ok, file:filename()} | {error, term()}.
fetch_package(Name, #source_spec{type = hex, ref = Version}) ->
    fetch_hex_package(Name, Version);
fetch_package(Name, #source_spec{type = git, url = Url, ref = Ref}) ->
    fetch_git_package(Name, Url, Ref);
fetch_package(_Name, #source_spec{type = Type}) ->
    {error, {unsupported_source_type, Type}}.

%% @doc Remove a package from the workspace.
%% Purges loaded modules, removes code paths, and deletes the directory.
-spec remove_package(atom()) -> ok | {error, term()}.
remove_package(Name) ->
    PackageDir = get_package_dir(Name),
    case filelib:is_dir(PackageDir) of
        true ->
            %% First purge all modules loaded from this package
            %% This is critical for hot code reloading - removing code paths
            %% alone doesn't unload already-loaded modules from memory
            purge_package_modules(Name),
            %% Then remove code paths
            remove_code_paths(Name),
            %% Finally delete directory
            delete_dir_recursive(PackageDir);
        false ->
            ok
    end.

%% @doc Delete a package's workspace directory without removing code paths.
%% Used during upgrades - we need to delete the old workspace to fetch
%% the new version, but keep old code paths until hot reload completes.
-spec delete_package(atom()) -> ok | {error, term()}.
delete_package(Name) ->
    PackageDir = get_package_dir(Name),
    case filelib:is_dir(PackageDir) of
        true ->
            delete_dir_recursive(PackageDir);
        false ->
            ok
    end.

%% @doc Get the path to a package's directory.
-spec get_package_path(atom()) -> {ok, file:filename()} | {error, not_found}.
get_package_path(Name) ->
    PackageDir = get_package_dir(Name),
    case filelib:is_dir(PackageDir) of
        true -> {ok, PackageDir};
        false -> {error, not_found}
    end.

%% @doc Get all ebin paths for a package and its dependencies.
-spec get_ebin_paths(atom()) -> [file:filename()].
get_ebin_paths(Name) ->
    PackageDir = get_package_dir(Name),
    BuildDir = filename:join(PackageDir, "_build/default/lib"),
    case filelib:is_dir(BuildDir) of
        true ->
            filelib:wildcard(filename:join(BuildDir, "*/ebin"));
        false ->
            []
    end.

%% @doc Clean up the entire workspace.
-spec cleanup() -> ok | {error, term()}.
cleanup() ->
    WorkspacePath = get_workspace_path(),
    delete_dir_recursive(WorkspacePath).

%% -----------------------------------------------------------------------------
%% Internal - Hex package fetching
%% -----------------------------------------------------------------------------

-spec fetch_hex_package(atom(), binary() | undefined) -> {ok, file:filename()} | {error, term()}.
fetch_hex_package(Name, Version) ->
    PackageDir = get_package_dir(Name),
    ok = filelib:ensure_dir(filename:join(PackageDir, "dummy")),

    %% Try rebar3 first (Erlang), fall back to mix (Elixir) on failure
    %% This approach works for both Erlang and Elixir packages since:
    %% - rebar3 can fetch hex packages regardless of language
    %% - If compilation fails with rebar3, mix will handle Elixir packages
    fetch_hex_erlang(Name, Version, PackageDir).

-spec fetch_hex_erlang(atom(), binary() | undefined, file:filename()) ->
    {ok, file:filename()} | {error, term()}.
fetch_hex_erlang(Name, Version, PackageDir) ->
    %% Create minimal rebar.config
    DepSpec = case Version of
        undefined -> io_lib:format("~p", [Name]);
        V -> io_lib:format("{~p, \"~s\"}", [Name, V])
    end,
    RebarConfig = io_lib:format(
        "{erl_opts, [debug_info]}.~n"
        "{deps, [~s]}.~n",
        [DepSpec]
    ),

    ConfigPath = filename:join(PackageDir, "rebar.config"),
    ok = file:write_file(ConfigPath, RebarConfig),

    %% Create minimal app.src so rebar3 doesn't complain
    SrcDir = filename:join(PackageDir, "src"),
    ok = filelib:ensure_dir(filename:join(SrcDir, "dummy")),
    AppSrc = io_lib:format(
        "{application, bc_gitops_fetch_~p, [~n"
        "    {description, \"Temporary fetch app\"},~n"
        "    {vsn, \"0.0.1\"},~n"
        "    {applications, [kernel, stdlib]}~n"
        "]}.~n",
        [Name]
    ),
    AppSrcPath = filename:join(SrcDir, io_lib:format("bc_gitops_fetch_~p.app.src", [Name])),
    ok = file:write_file(AppSrcPath, AppSrc),

    %% Run rebar3 to fetch and compile
    case run_cmd(PackageDir, "rebar3 get-deps && rebar3 compile") of
        {ok, _Output} ->
            %% Add code paths
            EbinPaths = get_ebin_paths(Name),
            lists:foreach(fun code:add_pathz/1, EbinPaths),
            {ok, PackageDir};
        {error, {exit_code, Code, Output}} ->
            %% Try with mix if rebar3 failed (might be Elixir package)
            case fetch_hex_elixir(Name, Version, PackageDir) of
                {ok, Path} -> {ok, Path};
                {error, _} -> {error, {rebar3_failed, Code, Output}}
            end
    end.

-spec fetch_hex_elixir(atom(), binary() | undefined, file:filename()) ->
    {ok, file:filename()} | {error, term()}.
fetch_hex_elixir(Name, Version, PackageDir) ->
    %% Check if mix is available
    case os:find_executable("mix") of
        false ->
            {error, mix_not_found};
        _MixPath ->
            do_fetch_hex_elixir(Name, Version, PackageDir)
    end.

-spec do_fetch_hex_elixir(atom(), binary() | undefined, file:filename()) ->
    {ok, file:filename()} | {error, term()}.
do_fetch_hex_elixir(Name, Version, PackageDir) ->
    %% Create minimal mix.exs
    DepSpec = case Version of
        undefined -> io_lib:format("{:~p, \">= 0.0.0\"}", [Name]);
        V -> io_lib:format("{:~p, \"~s\"}", [Name, V])
    end,
    MixExs = io_lib:format(
        "defmodule BcGitopsFetch.MixProject do~n"
        "  use Mix.Project~n"
        "~n"
        "  def project do~n"
        "    [~n"
        "      app: :bc_gitops_fetch_~p,~n"
        "      version: \"0.0.1\",~n"
        "      deps: deps()~n"
        "    ]~n"
        "  end~n"
        "~n"
        "  defp deps do~n"
        "    [~s]~n"
        "  end~n"
        "end~n",
        [Name, DepSpec]
    ),

    MixPath = filename:join(PackageDir, "mix.exs"),
    ok = file:write_file(MixPath, MixExs),

    %% Run mix to fetch and compile
    case run_cmd(PackageDir, "mix local.hex --force --if-missing && mix deps.get && mix compile") of
        {ok, _Output} ->
            %% Add code paths (mix uses _build/dev/lib by default)
            BuildDir = filename:join(PackageDir, "_build/dev/lib"),
            EbinPaths = filelib:wildcard(filename:join(BuildDir, "*/ebin")),
            lists:foreach(fun code:add_pathz/1, EbinPaths),
            {ok, PackageDir};
        {error, {exit_code, Code, Output}} ->
            {error, {mix_failed, Code, Output}}
    end.

%% -----------------------------------------------------------------------------
%% Internal - Git package fetching
%% -----------------------------------------------------------------------------

-spec fetch_git_package(atom(), binary(), binary() | undefined) ->
    {ok, file:filename()} | {error, term()}.
fetch_git_package(Name, Url, Ref) ->
    PackageDir = get_package_dir(Name),

    %% Emit clone start telemetry
    emit_telemetry(?TELEMETRY_GIT_CLONE_START, #{}, #{
        app => Name,
        url => Url,
        ref => Ref
    }),
    CloneStart = erlang:monotonic_time(),

    %% Check if directory already exists
    Result = case filelib:is_dir(PackageDir) of
        true ->
            %% Directory exists - check if it's a git repo and update it
            GitDir = filename:join(PackageDir, ".git"),
            case filelib:is_dir(GitDir) of
                true ->
                    %% It's a git repo - fetch and checkout the ref
                    update_git_package(PackageDir, Ref);
                false ->
                    %% Not a git repo - remove and clone fresh
                    remove_directory(PackageDir),
                    clone_git_package(PackageDir, Url, Ref)
            end;
        false ->
            %% Directory doesn't exist - clone fresh
            clone_git_package(PackageDir, Url, Ref)
    end,

    CloneDuration = erlang:monotonic_time() - CloneStart,
    case Result of
        ok ->
            emit_telemetry(?TELEMETRY_GIT_CLONE_STOP, #{duration => CloneDuration}, #{
                app => Name,
                url => Url,
                ref => Ref,
                status => success
            }),
            compile_git_package(Name, PackageDir);
        {error, Reason} ->
            emit_telemetry(?TELEMETRY_GIT_CLONE_STOP, #{duration => CloneDuration}, #{
                app => Name,
                url => Url,
                ref => Ref,
                status => failed,
                error => Reason
            }),
            {error, {git_clone_failed, Reason}}
    end.

%% Clone a git repository
-spec clone_git_package(file:filename(), binary(), binary() | undefined) -> ok | {error, term()}.
clone_git_package(PackageDir, Url, Ref) ->
    CloneCmd = case Ref of
        undefined ->
            io_lib:format("git clone --depth 1 \"~s\" \"~s\"", [Url, PackageDir]);
        R ->
            io_lib:format("git clone --depth 1 --branch \"~s\" \"~s\" \"~s\"", [R, Url, PackageDir])
    end,
    case run_cmd("/tmp", CloneCmd) of
        {ok, _} -> ok;
        {error, Reason} -> {error, Reason}
    end.

%% Update an existing git repository
-spec update_git_package(file:filename(), binary() | undefined) -> ok | {error, term()}.
update_git_package(PackageDir, Ref) ->
    %% Fetch latest changes
    case run_cmd(PackageDir, "git fetch --all --prune") of
        {ok, _} ->
            %% Checkout the specified ref (or default branch)
            CheckoutCmd = case Ref of
                undefined ->
                    "git checkout HEAD && git pull";
                R ->
                    io_lib:format("git checkout \"~s\" && git pull origin \"~s\"", [R, R])
            end,
            case run_cmd(PackageDir, CheckoutCmd) of
                {ok, _} -> ok;
                {error, Reason} -> {error, {checkout_failed, Reason}}
            end;
        {error, Reason} ->
            {error, {fetch_failed, Reason}}
    end.

%% Remove a directory recursively
-spec remove_directory(file:filename()) -> ok.
remove_directory(Dir) ->
    Cmd = io_lib:format("rm -rf \"~s\"", [Dir]),
    run_cmd("/tmp", Cmd),
    ok.

-spec compile_git_package(atom(), file:filename()) -> {ok, file:filename()} | {error, term()}.
compile_git_package(Name, PackageDir) ->
    case detect_project_type(PackageDir) of
        rebar3 ->
            compile_rebar3_project(Name, PackageDir);
        mix ->
            compile_mix_project(Name, PackageDir);
        erlang_mk ->
            compile_erlang_mk_project(Name, PackageDir);
        unknown ->
            {error, {unknown_project_type, PackageDir}}
    end.

-spec detect_project_type(file:filename()) -> rebar3 | mix | erlang_mk | unknown.
detect_project_type(Dir) ->
    case filelib:is_file(filename:join(Dir, "mix.exs")) of
        true -> mix;
        false ->
            case filelib:is_file(filename:join(Dir, "rebar.config")) of
                true -> rebar3;
                false ->
                    case filelib:is_file(filename:join(Dir, "Makefile")) of
                        true -> erlang_mk;
                        false -> unknown
                    end
            end
    end.

-spec compile_rebar3_project(atom(), file:filename()) -> {ok, file:filename()} | {error, term()}.
compile_rebar3_project(Name, PackageDir) ->
    %% Deps fetch
    emit_telemetry(?TELEMETRY_DEPS_START, #{}, #{app => Name, tool => rebar3}),
    DepsStart = erlang:monotonic_time(),
    case run_cmd(PackageDir, "rebar3 get-deps") of
        {ok, _} ->
            DepsDuration = erlang:monotonic_time() - DepsStart,
            emit_telemetry(?TELEMETRY_DEPS_STOP, #{duration => DepsDuration}, #{app => Name, tool => rebar3, status => success}),

            %% Build
            emit_telemetry(?TELEMETRY_BUILD_START, #{}, #{app => Name, tool => rebar3}),
            BuildStart = erlang:monotonic_time(),
            case run_cmd(PackageDir, "rebar3 compile") of
                {ok, _} ->
                    BuildDuration = erlang:monotonic_time() - BuildStart,
                    emit_telemetry(?TELEMETRY_BUILD_STOP, #{duration => BuildDuration}, #{app => Name, tool => rebar3, status => success}),

                    %% Load code paths
                    EbinPaths = get_ebin_paths(Name),
                    lists:foreach(fun code:add_pathz/1, EbinPaths),
                    emit_telemetry(?TELEMETRY_CODE_LOAD, #{}, #{app => Name, paths_count => length(EbinPaths)}),
                    {ok, PackageDir};
                {error, Reason} ->
                    BuildDuration = erlang:monotonic_time() - BuildStart,
                    emit_telemetry(?TELEMETRY_BUILD_STOP, #{duration => BuildDuration}, #{app => Name, tool => rebar3, status => failed, error => Reason}),
                    {error, {rebar3_compile_failed, Reason}}
            end;
        {error, Reason} ->
            DepsDuration = erlang:monotonic_time() - DepsStart,
            emit_telemetry(?TELEMETRY_DEPS_STOP, #{duration => DepsDuration}, #{app => Name, tool => rebar3, status => failed, error => Reason}),
            {error, {rebar3_deps_failed, Reason}}
    end.

-spec compile_mix_project(atom(), file:filename()) -> {ok, file:filename()} | {error, term()}.
compile_mix_project(Name, PackageDir) ->
    %% Deps fetch
    emit_telemetry(?TELEMETRY_DEPS_START, #{}, #{app => Name, tool => mix}),
    DepsStart = erlang:monotonic_time(),
    case run_cmd(PackageDir, "mix deps.get") of
        {ok, _} ->
            DepsDuration = erlang:monotonic_time() - DepsStart,
            emit_telemetry(?TELEMETRY_DEPS_STOP, #{duration => DepsDuration}, #{app => Name, tool => mix, status => success}),

            %% Build
            emit_telemetry(?TELEMETRY_BUILD_START, #{}, #{app => Name, tool => mix}),
            BuildStart = erlang:monotonic_time(),
            case run_cmd(PackageDir, "mix compile") of
                {ok, _} ->
                    BuildDuration = erlang:monotonic_time() - BuildStart,
                    emit_telemetry(?TELEMETRY_BUILD_STOP, #{duration => BuildDuration}, #{app => Name, tool => mix, status => success}),

                    %% Load code paths
                    BuildDir = filename:join(PackageDir, "_build/dev/lib"),
                    EbinPaths = filelib:wildcard(filename:join(BuildDir, "*/ebin")),
                    lists:foreach(fun code:add_pathz/1, EbinPaths),
                    emit_telemetry(?TELEMETRY_CODE_LOAD, #{}, #{app => Name, paths_count => length(EbinPaths)}),
                    {ok, PackageDir};
                {error, Reason} ->
                    BuildDuration = erlang:monotonic_time() - BuildStart,
                    emit_telemetry(?TELEMETRY_BUILD_STOP, #{duration => BuildDuration}, #{app => Name, tool => mix, status => failed, error => Reason}),
                    {error, {mix_compile_failed, Reason}}
            end;
        {error, Reason} ->
            DepsDuration = erlang:monotonic_time() - DepsStart,
            emit_telemetry(?TELEMETRY_DEPS_STOP, #{duration => DepsDuration}, #{app => Name, tool => mix, status => failed, error => Reason}),
            {error, {mix_deps_failed, Reason}}
    end.

-spec compile_erlang_mk_project(atom(), file:filename()) -> {ok, file:filename()} | {error, term()}.
compile_erlang_mk_project(_Name, PackageDir) ->
    case run_cmd(PackageDir, "make") of
        {ok, _} ->
            EbinPath = filename:join(PackageDir, "ebin"),
            code:add_pathz(EbinPath),
            {ok, PackageDir};
        {error, Reason} ->
            {error, {erlang_mk_compile_failed, Reason}}
    end.

%% -----------------------------------------------------------------------------
%% Internal - Utilities
%% -----------------------------------------------------------------------------

-spec get_workspace_path() -> file:filename().
get_workspace_path() ->
    try
        persistent_term:get(?WORKSPACE_KEY)
    catch
        error:badarg -> ?DEFAULT_WORKSPACE
    end.

-spec get_package_dir(atom()) -> file:filename().
get_package_dir(Name) ->
    filename:join(get_workspace_path(), atom_to_list(Name)).

-spec run_cmd(file:filename(), string()) -> {ok, string()} | {error, term()}.
run_cmd(Dir, Cmd) ->
    FullCmd = lists:flatten(io_lib:format("cd \"~s\" && ~s 2>&1", [Dir, Cmd])),
    Port = open_port({spawn_executable, "/bin/sh"}, [
        {args, ["-c", FullCmd]},
        exit_status,
        binary,
        stderr_to_stdout
    ]),
    collect_port_output(Port, []).

-spec collect_port_output(port(), [binary()]) -> {ok, string()} | {error, term()}.
collect_port_output(Port, Acc) ->
    receive
        {Port, {data, Data}} ->
            collect_port_output(Port, [Data | Acc]);
        {Port, {exit_status, 0}} ->
            Output = binary_to_list(iolist_to_binary(lists:reverse(Acc))),
            {ok, Output};
        {Port, {exit_status, Code}} ->
            Output = binary_to_list(iolist_to_binary(lists:reverse(Acc))),
            {error, {exit_code, Code, Output}}
    after 300000 ->  %% 5 minute timeout
        port_close(Port),
        {error, timeout}
    end.

-spec remove_code_paths(atom()) -> ok.
remove_code_paths(Name) ->
    PackageDir = get_package_dir(Name),
    CurrentPaths = code:get_path(),
    PathsToRemove = [P || P <- CurrentPaths, lists:prefix(PackageDir, P)],
    lists:foreach(fun code:del_path/1, PathsToRemove),
    ok.

%% @doc Purge all modules loaded from a package's ebin directories.
%% This is necessary for proper hot code reloading - removing code paths alone
%% doesn't unload already-loaded modules from memory.
-spec purge_package_modules(atom()) -> ok.
purge_package_modules(Name) ->
    PackageDir = get_package_dir(Name),
    %% Find all ebin directories for this package (rebar3 style)
    EbinPaths1 = filelib:wildcard(filename:join([PackageDir, "_build", "default", "lib", "*", "ebin"])),
    %% Also check mix dev build (mix style)
    EbinPaths2 = filelib:wildcard(filename:join([PackageDir, "_build", "dev", "lib", "*", "ebin"])),
    AllEbinPaths = EbinPaths1 ++ EbinPaths2,

    %% Get all module names from .beam files in these directories
    Modules = lists:flatmap(fun get_modules_in_ebin/1, AllEbinPaths),

    %% Emit telemetry for module purging
    emit_telemetry(?TELEMETRY_CODE_PURGE, #{}, #{
        app => Name,
        module_count => length(Modules),
        modules => Modules
    }),

    %% Purge each module (soft_purge first, then force purge if needed)
    lists:foreach(fun do_purge_module/1, Modules),
    ok.

-spec get_modules_in_ebin(file:filename()) -> [module()].
get_modules_in_ebin(EbinPath) ->
    BeamFiles = filelib:wildcard(filename:join(EbinPath, "*.beam")),
    [list_to_atom(filename:basename(F, ".beam")) || F <- BeamFiles].

-spec do_purge_module(module()) -> ok.
do_purge_module(Module) ->
    %% First try soft_purge - this succeeds if no process is running old code
    case code:soft_purge(Module) of
        true ->
            ok;
        false ->
            %% Soft purge failed (processes running old code), force purge
            %% This will kill any processes running the old code
            code:purge(Module),
            ok
    end.

-spec delete_dir_recursive(file:filename()) -> ok | {error, term()}.
delete_dir_recursive(Dir) ->
    case filelib:is_dir(Dir) of
        true ->
            Cmd = io_lib:format("rm -rf \"~s\"", [Dir]),
            case os:cmd(lists:flatten(Cmd)) of
                [] -> ok;
                Error -> {error, Error}
            end;
        false ->
            ok
    end.

%% -----------------------------------------------------------------------------
%% Telemetry
%% -----------------------------------------------------------------------------

-spec emit_telemetry([atom()], map(), map()) -> ok.
emit_telemetry(EventName, Measurements, Metadata) ->
    telemetry:execute(EventName, Measurements, Metadata).
