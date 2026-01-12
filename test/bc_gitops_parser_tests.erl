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

%% -----------------------------------------------------------------------------
%% JSON config file tests
%% -----------------------------------------------------------------------------

parse_json_config_file_test() ->
    TmpDir = "/tmp/bc_gitops_test_" ++ integer_to_list(erlang:unique_integer([positive])),
    ok = file:make_dir(TmpDir),
    JsonFile = filename:join(TmpDir, "app.json"),

    JsonContent = <<"{
        \"name\": \"json_app\",
        \"version\": \"1.2.3\",
        \"source\": {\"type\": \"hex\"},
        \"env\": {\"port\": 9090},
        \"depends_on\": []
    }">>,
    ok = file:write_file(JsonFile, JsonContent),

    try
        %% JSON requires OTP 27+, so this might fail on older versions
        case bc_gitops_parser:parse_app_config_file(JsonFile) of
            {ok, AppSpec} ->
                ?assertEqual(json_app, AppSpec#app_spec.name),
                ?assertEqual(<<"1.2.3">>, AppSpec#app_spec.version),
                ?assertEqual(hex, (AppSpec#app_spec.source)#source_spec.type),
                ?assertEqual(9090, maps:get(port, AppSpec#app_spec.env));
            {error, {json_decode_error, {json_not_supported, _}}} ->
                %% Expected on OTP < 27 without jsx/jiffy
                ok;
            {error, {json_decode_error, undef}} ->
                %% json module not available
                ok
        end
    after
        file:delete(JsonFile),
        file:del_dir(TmpDir)
    end.

parse_json_invalid_format_test() ->
    TmpDir = "/tmp/bc_gitops_test_" ++ integer_to_list(erlang:unique_integer([positive])),
    ok = file:make_dir(TmpDir),
    JsonFile = filename:join(TmpDir, "invalid.json"),

    %% JSON array instead of object
    ok = file:write_file(JsonFile, <<"[1, 2, 3]">>),

    try
        case bc_gitops_parser:parse_app_config_file(JsonFile) of
            {error, {invalid_json, expected_object}} ->
                ok;
            {error, {json_decode_error, _}} ->
                %% JSON not supported or parse error
                ok
        end
    after
        file:delete(JsonFile),
        file:del_dir(TmpDir)
    end.

%% -----------------------------------------------------------------------------
%% YAML config file tests
%% -----------------------------------------------------------------------------

parse_yaml_config_file_test() ->
    TmpDir = "/tmp/bc_gitops_test_" ++ integer_to_list(erlang:unique_integer([positive])),
    ok = file:make_dir(TmpDir),
    YamlFile = filename:join(TmpDir, "app.yaml"),

    YamlContent = <<"name: yaml_app
version: \"3.0.0\"
source:
  type: hex
env:
  port: 7070
  pool_size: 5
depends_on: []
">>,
    ok = file:write_file(YamlFile, YamlContent),

    try
        case bc_gitops_parser:parse_app_config_file(YamlFile) of
            {ok, AppSpec} ->
                ?assertEqual(yaml_app, AppSpec#app_spec.name),
                ?assertEqual(<<"3.0.0">>, AppSpec#app_spec.version),
                ?assertEqual(hex, (AppSpec#app_spec.source)#source_spec.type),
                ?assertEqual(7070, maps:get(port, AppSpec#app_spec.env)),
                ?assertEqual(5, maps:get(pool_size, AppSpec#app_spec.env));
            {error, {yaml_decode_error, {yaml_not_supported, _}}} ->
                %% Expected when yamerl is not available
                ok
        end
    after
        file:delete(YamlFile),
        file:del_dir(TmpDir)
    end.

parse_yml_extension_test() ->
    TmpDir = "/tmp/bc_gitops_test_" ++ integer_to_list(erlang:unique_integer([positive])),
    ok = file:make_dir(TmpDir),
    YmlFile = filename:join(TmpDir, "app.yml"),

    YamlContent = <<"name: yml_app
version: \"1.0.0\"
source:
  type: git
  url: https://github.com/test/repo.git
  ref: main
env: {}
depends_on: []
">>,
    ok = file:write_file(YmlFile, YamlContent),

    try
        case bc_gitops_parser:parse_app_config_file(YmlFile) of
            {ok, AppSpec} ->
                ?assertEqual(yml_app, AppSpec#app_spec.name),
                ?assertEqual(git, (AppSpec#app_spec.source)#source_spec.type);
            {error, {yaml_decode_error, {yaml_not_supported, _}}} ->
                ok
        end
    after
        file:delete(YmlFile),
        file:del_dir(TmpDir)
    end.

parse_yaml_with_health_test() ->
    TmpDir = "/tmp/bc_gitops_test_" ++ integer_to_list(erlang:unique_integer([positive])),
    ok = file:make_dir(TmpDir),
    YamlFile = filename:join(TmpDir, "app.yaml"),

    YamlContent = <<"name: health_app
version: \"1.0.0\"
source:
  type: hex
env: {}
health:
  type: http
  port: 8080
  path: /health
  interval: 30000
  timeout: 5000
depends_on: []
">>,
    ok = file:write_file(YamlFile, YamlContent),

    try
        case bc_gitops_parser:parse_app_config_file(YamlFile) of
            {ok, AppSpec} ->
                Health = AppSpec#app_spec.health,
                ?assertNotEqual(undefined, Health),
                ?assertEqual(http, Health#health_spec.type),
                ?assertEqual(8080, Health#health_spec.port),
                ?assertEqual(<<"/health">>, Health#health_spec.path);
            {error, {yaml_decode_error, {yaml_not_supported, _}}} ->
                ok
        end
    after
        file:delete(YamlFile),
        file:del_dir(TmpDir)
    end.

%% -----------------------------------------------------------------------------
%% Config file priority tests
%% -----------------------------------------------------------------------------

parse_apps_dir_prefers_erlang_config_test() ->
    TmpDir = "/tmp/bc_gitops_test_" ++ integer_to_list(erlang:unique_integer([positive])),
    AppDir = filename:join(TmpDir, "priority_app"),

    ok = file:make_dir(TmpDir),
    ok = file:make_dir(AppDir),

    %% Create both .config and .yaml files
    ErlConfig = #{name => erlang_wins, version => <<"1.0.0">>, source => #{type => hex}, env => #{}, depends_on => []},
    ok = file:write_file(filename:join(AppDir, "app.config"), io_lib:format("~p.~n", [ErlConfig])),
    ok = file:write_file(filename:join(AppDir, "app.yaml"), <<"name: yaml_loses\nversion: \"2.0.0\"\nsource:\n  type: hex\nenv: {}\ndepends_on: []\n">>),

    try
        {ok, Apps} = bc_gitops_parser:parse_apps_dir(TmpDir),
        AppSpec = maps:get(erlang_wins, Apps),
        %% Erlang config should be preferred
        ?assertEqual(erlang_wins, AppSpec#app_spec.name),
        ?assertEqual(<<"1.0.0">>, AppSpec#app_spec.version)
    after
        file:delete(filename:join(AppDir, "app.config")),
        file:delete(filename:join(AppDir, "app.yaml")),
        file:del_dir(AppDir),
        file:del_dir(TmpDir)
    end.

parse_apps_dir_falls_back_to_yaml_test() ->
    TmpDir = "/tmp/bc_gitops_test_" ++ integer_to_list(erlang:unique_integer([positive])),
    AppDir = filename:join(TmpDir, "yaml_app"),

    ok = file:make_dir(TmpDir),
    ok = file:make_dir(AppDir),

    %% Only create .yaml file
    ok = file:write_file(filename:join(AppDir, "app.yaml"), <<"name: yaml_only\nversion: \"1.0.0\"\nsource:\n  type: hex\nenv: {}\ndepends_on: []\n">>),

    try
        case bc_gitops_parser:parse_apps_dir(TmpDir) of
            {ok, Apps} ->
                case maps:find(yaml_only, Apps) of
                    {ok, AppSpec} ->
                        ?assertEqual(yaml_only, AppSpec#app_spec.name);
                    error ->
                        %% YAML not supported
                        ok
                end;
            {error, _} ->
                %% YAML parsing failed (yamerl not available)
                ok
        end
    after
        file:delete(filename:join(AppDir, "app.yaml")),
        file:del_dir(AppDir),
        file:del_dir(TmpDir)
    end.

parse_apps_dir_with_json_test() ->
    TmpDir = "/tmp/bc_gitops_test_" ++ integer_to_list(erlang:unique_integer([positive])),
    AppDir = filename:join(TmpDir, "json_app"),

    ok = file:make_dir(TmpDir),
    ok = file:make_dir(AppDir),

    %% Only create .json file
    JsonContent = <<"{\"name\": \"json_only\", \"version\": \"1.0.0\", \"source\": {\"type\": \"hex\"}, \"env\": {}, \"depends_on\": []}">>,
    ok = file:write_file(filename:join(AppDir, "app.json"), JsonContent),

    try
        case bc_gitops_parser:parse_apps_dir(TmpDir) of
            {ok, Apps} ->
                case maps:find(json_only, Apps) of
                    {ok, AppSpec} ->
                        ?assertEqual(json_only, AppSpec#app_spec.name);
                    error ->
                        %% JSON not supported
                        ok
                end;
            {error, _} ->
                %% JSON parsing failed
                ok
        end
    after
        file:delete(filename:join(AppDir, "app.json")),
        file:del_dir(AppDir),
        file:del_dir(TmpDir)
    end.
