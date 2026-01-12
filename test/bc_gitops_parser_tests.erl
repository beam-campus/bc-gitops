%%% @doc Unit tests for bc_gitops_parser module.
-module(bc_gitops_parser_tests).

-include_lib("eunit/include/eunit.hrl").
-include("bc_gitops.hrl").

%% -----------------------------------------------------------------------------
%% Test fixtures
%% -----------------------------------------------------------------------------

valid_config() ->
    #{
        name => my_app,
        version => <<"1.0.0">>,
        source => #{
            type => hex
        },
        env => #{
            port => 8080
        },
        depends_on => []
    }.

valid_config_with_health() ->
    maps:merge(valid_config(), #{
        health => #{
            type => http,
            port => 8080,
            path => <<"/health">>,
            interval => 30000,
            timeout => 5000
        }
    }).

valid_config_with_git_source() ->
    maps:merge(valid_config(), #{
        source => #{
            type => git,
            url => <<"https://github.com/org/repo.git">>,
            ref => <<"main">>
        }
    }).

%% -----------------------------------------------------------------------------
%% parse_app_config/1 tests
%% -----------------------------------------------------------------------------

parse_valid_config_test() ->
    Config = valid_config(),
    {ok, AppSpec} = bc_gitops_parser:parse_app_config(Config),

    ?assertEqual(my_app, AppSpec#app_spec.name),
    ?assertEqual(<<"1.0.0">>, AppSpec#app_spec.version),
    ?assertEqual(hex, (AppSpec#app_spec.source)#source_spec.type),
    ?assertEqual(#{port => 8080}, AppSpec#app_spec.env),
    ?assertEqual([], AppSpec#app_spec.depends_on),
    ?assertEqual(undefined, AppSpec#app_spec.health).

parse_config_with_health_test() ->
    Config = valid_config_with_health(),
    {ok, AppSpec} = bc_gitops_parser:parse_app_config(Config),

    Health = AppSpec#app_spec.health,
    ?assertNotEqual(undefined, Health),
    ?assertEqual(http, Health#health_spec.type),
    ?assertEqual(8080, Health#health_spec.port),
    ?assertEqual(<<"/health">>, Health#health_spec.path),
    ?assertEqual(30000, Health#health_spec.interval),
    ?assertEqual(5000, Health#health_spec.timeout).

parse_config_with_git_source_test() ->
    Config = valid_config_with_git_source(),
    {ok, AppSpec} = bc_gitops_parser:parse_app_config(Config),

    Source = AppSpec#app_spec.source,
    ?assertEqual(git, Source#source_spec.type),
    ?assertEqual(<<"https://github.com/org/repo.git">>, Source#source_spec.url),
    ?assertEqual(<<"main">>, Source#source_spec.ref).

parse_config_missing_name_test() ->
    Config = maps:remove(name, valid_config()),
    Result = bc_gitops_parser:parse_app_config(Config),

    ?assertMatch({error, {missing_field, name}}, Result).

parse_config_missing_version_test() ->
    Config = maps:remove(version, valid_config()),
    Result = bc_gitops_parser:parse_app_config(Config),

    ?assertMatch({error, {missing_field, version}}, Result).

parse_config_name_as_binary_test() ->
    Config = maps:put(name, <<"my_app">>, valid_config()),
    {ok, AppSpec} = bc_gitops_parser:parse_app_config(Config),

    ?assertEqual(my_app, AppSpec#app_spec.name).

parse_config_version_as_list_test() ->
    Config = maps:put(version, "1.0.0", valid_config()),
    {ok, AppSpec} = bc_gitops_parser:parse_app_config(Config),

    ?assertEqual(<<"1.0.0">>, AppSpec#app_spec.version).

parse_config_with_dependencies_test() ->
    Config = maps:put(depends_on, [dep1, dep2, dep3], valid_config()),
    {ok, AppSpec} = bc_gitops_parser:parse_app_config(Config),

    ?assertEqual([dep1, dep2, dep3], AppSpec#app_spec.depends_on).

%% -----------------------------------------------------------------------------
%% parse_apps_dir/1 tests
%% -----------------------------------------------------------------------------

parse_apps_dir_nonexistent_test() ->
    Result = bc_gitops_parser:parse_apps_dir("/nonexistent/path"),
    ?assertMatch({error, {not_a_directory, _}}, Result).

parse_apps_dir_empty_test() ->
    %% Create a temporary empty directory
    TmpDir = "/tmp/bc_gitops_test_" ++ integer_to_list(erlang:unique_integer([positive])),
    ok = file:make_dir(TmpDir),

    try
        {ok, Apps} = bc_gitops_parser:parse_apps_dir(TmpDir),
        ?assertEqual(#{}, Apps)
    after
        file:del_dir(TmpDir)
    end.

parse_apps_dir_with_app_test() ->
    %% Create a temporary directory structure
    TmpDir = "/tmp/bc_gitops_test_" ++ integer_to_list(erlang:unique_integer([positive])),
    AppDir = filename:join(TmpDir, "test_app"),
    ConfigFile = filename:join(AppDir, "app.config"),

    ok = file:make_dir(TmpDir),
    ok = file:make_dir(AppDir),

    Config = #{
        name => test_app,
        version => <<"2.0.0">>,
        source => #{type => hex},
        env => #{},
        depends_on => []
    },
    ConfigStr = io_lib:format("~p.~n", [Config]),
    ok = file:write_file(ConfigFile, ConfigStr),

    try
        {ok, Apps} = bc_gitops_parser:parse_apps_dir(TmpDir),
        ?assertEqual(1, maps:size(Apps)),
        ?assert(maps:is_key(test_app, Apps)),

        AppSpec = maps:get(test_app, Apps),
        ?assertEqual(test_app, AppSpec#app_spec.name),
        ?assertEqual(<<"2.0.0">>, AppSpec#app_spec.version)
    after
        file:delete(ConfigFile),
        file:del_dir(AppDir),
        file:del_dir(TmpDir)
    end.

parse_apps_dir_skips_non_directories_test() ->
    TmpDir = "/tmp/bc_gitops_test_" ++ integer_to_list(erlang:unique_integer([positive])),
    ok = file:make_dir(TmpDir),

    %% Create a regular file (not a directory)
    RegularFile = filename:join(TmpDir, "not_a_dir.txt"),
    ok = file:write_file(RegularFile, <<"content">>),

    try
        {ok, Apps} = bc_gitops_parser:parse_apps_dir(TmpDir),
        ?assertEqual(#{}, Apps)  %% Should skip the file
    after
        file:delete(RegularFile),
        file:del_dir(TmpDir)
    end.

parse_apps_dir_skips_dirs_without_config_test() ->
    TmpDir = "/tmp/bc_gitops_test_" ++ integer_to_list(erlang:unique_integer([positive])),
    AppDir = filename:join(TmpDir, "no_config_app"),

    ok = file:make_dir(TmpDir),
    ok = file:make_dir(AppDir),

    try
        {ok, Apps} = bc_gitops_parser:parse_apps_dir(TmpDir),
        ?assertEqual(#{}, Apps)  %% Should skip directory without config
    after
        file:del_dir(AppDir),
        file:del_dir(TmpDir)
    end.
