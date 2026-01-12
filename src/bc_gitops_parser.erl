%%% @doc Configuration parser for bc_gitops.
%%%
%%% This module parses application specifications from the git repository.
%%% It supports multiple configuration formats:
%%%
%%% <ul>
%%% <li>Erlang term files (`.app.config`, `.config`)</li>
%%% <li>JSON files (`.json`)</li>
%%% </ul>
%%%
%%% == Directory Structure ==
%%%
%%% The apps directory should contain one subdirectory per application:
%%% ```
%%% apps/
%%%   my_app/
%%%     app.config    # Application specification
%%%   another_app/
%%%     app.json      # JSON format also supported
%%% '''
%%%
%%% == Configuration Format (Erlang) ==
%%%
%%% ```
%%% #{
%%%     name => my_app,
%%%     version => <<"1.0.0">>,
%%%     source => #{
%%%         type => hex,
%%%         %% or: type => git, url => <<"...">>, ref => <<"main">>
%%%         %% or: type => release, url => <<"...">>, sha256 => <<"...">>
%%%     },
%%%     env => #{
%%%         key => value
%%%     },
%%%     depends_on => [other_app],
%%%     health => #{
%%%         type => http,
%%%         port => 8080,
%%%         path => <<"/health">>,
%%%         interval => 30000,
%%%         timeout => 5000
%%%     }
%%% }.
%%% '''
%%% @end
-module(bc_gitops_parser).

-include("bc_gitops.hrl").

%% API
-export([
    parse_apps_dir/1,
    parse_app_config/1,
    parse_app_config_file/1
]).

%% -----------------------------------------------------------------------------
%% API
%% -----------------------------------------------------------------------------

%% @doc Parse all application configs from an apps directory.
%%
%% Returns a map of app_name => app_spec.
-spec parse_apps_dir(file:filename()) -> {ok, #{atom() => #app_spec{}}} | {error, term()}.
parse_apps_dir(AppsDir) ->
    case filelib:is_dir(AppsDir) of
        true ->
            case file:list_dir(AppsDir) of
                {ok, Entries} ->
                    parse_app_entries(AppsDir, Entries, #{});
                {error, Reason} ->
                    {error, {list_dir_failed, Reason}}
            end;
        false ->
            {error, {not_a_directory, AppsDir}}
    end.

%% @doc Parse a single application configuration from a map.
-spec parse_app_config(map()) -> {ok, #app_spec{}} | {error, term()}.
parse_app_config(Config) when is_map(Config) ->
    try
        Name = get_required(name, Config, atom),
        Version = get_required(version, Config, binary),
        Source = parse_source_spec(maps:get(source, Config, #{})),
        Env = maps:get(env, Config, #{}),
        DependsOn = maps:get(depends_on, Config, []),
        Health = parse_health_spec(maps:get(health, Config, undefined)),

        AppSpec = #app_spec{
            name = Name,
            version = Version,
            source = Source,
            env = Env,
            depends_on = DependsOn,
            health = Health
        },
        {ok, AppSpec}
    catch
        throw:{missing_field, Field} ->
            {error, {missing_field, Field}};
        throw:{invalid_type, Field, Expected, Got} ->
            {error, {invalid_type, Field, Expected, Got}};
        error:Reason ->
            {error, {parse_error, Reason}}
    end.

%% @doc Parse an application configuration from a file.
%%
%% Supports `.config`, `.app.config` (Erlang terms) and `.json` files.
-spec parse_app_config_file(file:filename()) -> {ok, #app_spec{}} | {error, term()}.
parse_app_config_file(FilePath) ->
    case filename:extension(FilePath) of
        ".json" ->
            parse_json_config_file(FilePath);
        _ ->
            parse_erlang_config_file(FilePath)
    end.

%% -----------------------------------------------------------------------------
%% Internal functions - Directory parsing
%% -----------------------------------------------------------------------------

-spec parse_app_entries(file:filename(), [string()], map()) -> {ok, map()} | {error, term()}.
parse_app_entries(_AppsDir, [], Acc) ->
    {ok, Acc};
parse_app_entries(AppsDir, [Entry | Rest], Acc) ->
    EntryPath = filename:join(AppsDir, Entry),
    case filelib:is_dir(EntryPath) of
        true ->
            case find_and_parse_app_config(EntryPath) of
                {ok, AppSpec} ->
                    NewAcc = maps:put(AppSpec#app_spec.name, AppSpec, Acc),
                    parse_app_entries(AppsDir, Rest, NewAcc);
                {error, no_config} ->
                    %% Skip directories without config
                    parse_app_entries(AppsDir, Rest, Acc);
                {error, Reason} ->
                    {error, {Entry, Reason}}
            end;
        false ->
            %% Skip non-directories
            parse_app_entries(AppsDir, Rest, Acc)
    end.

-spec find_and_parse_app_config(file:filename()) -> {ok, #app_spec{}} | {error, term()}.
find_and_parse_app_config(AppDir) ->
    %% Look for config files in order of preference
    ConfigFiles = [
        filename:join(AppDir, "app.config"),
        filename:join(AppDir, "app.json"),
        filename:join(AppDir, "config.json"),
        filename:join(AppDir, "config")
    ],
    find_first_config(ConfigFiles).

-spec find_first_config([file:filename()]) -> {ok, #app_spec{}} | {error, term()}.
find_first_config([]) ->
    {error, no_config};
find_first_config([File | Rest]) ->
    case filelib:is_file(File) of
        true ->
            parse_app_config_file(File);
        false ->
            find_first_config(Rest)
    end.

%% -----------------------------------------------------------------------------
%% Internal functions - File parsing
%% -----------------------------------------------------------------------------

-spec parse_erlang_config_file(file:filename()) -> {ok, #app_spec{}} | {error, term()}.
parse_erlang_config_file(FilePath) ->
    case file:consult(FilePath) of
        {ok, [Config]} when is_map(Config) ->
            parse_app_config(Config);
        {ok, [Config]} when is_list(Config) ->
            %% Convert proplist to map
            parse_app_config(maps:from_list(Config));
        {ok, _Other} ->
            {error, {invalid_format, expected_single_map_or_proplist}};
        {error, Reason} ->
            {error, {file_read_error, Reason}}
    end.

-spec parse_json_config_file(file:filename()) -> {ok, #app_spec{}} | {error, term()}.
parse_json_config_file(FilePath) ->
    case file:read_file(FilePath) of
        {ok, Content} ->
            case json_decode(Content) of
                {ok, Config} when is_map(Config) ->
                    %% Convert string keys to atoms
                    AtomConfig = atomize_keys(Config),
                    parse_app_config(AtomConfig);
                {ok, _Other} ->
                    {error, {invalid_json, expected_object}};
                {error, Reason} ->
                    {error, {json_decode_error, Reason}}
            end;
        {error, Reason} ->
            {error, {file_read_error, Reason}}
    end.

%% @doc Decode JSON using OTP 27+ json module or fallback.
-spec json_decode(binary()) -> {ok, term()} | {error, term()}.
json_decode(Binary) ->
    try
        %% OTP 27+ has json module
        Decoded = json:decode(Binary),
        {ok, Decoded}
    catch
        error:undef ->
            %% Fallback for older OTP: simple JSON subset parser
            simple_json_decode(Binary);
        error:Reason ->
            {error, Reason}
    end.

%% Simple JSON decoder for basic cases (objects and primitives)
-spec simple_json_decode(binary()) -> {ok, term()} | {error, term()}.
simple_json_decode(_Binary) ->
    %% This is a minimal fallback - in production, consider adding
    %% jsx or jiffy as an optional dependency
    {error, {json_not_supported, <<"OTP < 27 requires json module">>}}.

-spec atomize_keys(map()) -> map().
atomize_keys(Map) when is_map(Map) ->
    maps:fold(
        fun(K, V, Acc) ->
            Key = to_atom(K),
            Value = atomize_value(V),
            maps:put(Key, Value, Acc)
        end,
        #{},
        Map
    );
atomize_keys(Other) ->
    Other.

-spec atomize_value(term()) -> term().
atomize_value(V) when is_map(V) -> atomize_keys(V);
atomize_value(V) when is_list(V) -> [atomize_value(E) || E <- V];
atomize_value(V) -> V.

-spec to_atom(binary() | atom() | string()) -> atom().
to_atom(B) when is_binary(B) -> binary_to_atom(B, utf8);
to_atom(A) when is_atom(A) -> A;
to_atom(S) when is_list(S) -> list_to_atom(S).

%% -----------------------------------------------------------------------------
%% Internal functions - Spec parsing
%% -----------------------------------------------------------------------------

-spec parse_source_spec(map()) -> #source_spec{}.
parse_source_spec(Source) when is_map(Source) ->
    Type = maps:get(type, Source, hex),
    #source_spec{
        type = Type,
        url = maps:get(url, Source, undefined),
        sha256 = maps:get(sha256, Source, undefined),
        ref = maps:get(ref, Source, undefined)
    };
parse_source_spec(_) ->
    #source_spec{type = hex}.

-spec parse_health_spec(map() | undefined) -> #health_spec{} | undefined.
parse_health_spec(undefined) ->
    undefined;
parse_health_spec(Health) when is_map(Health) ->
    #health_spec{
        type = maps:get(type, Health, http),
        port = maps:get(port, Health, 8080),
        path = maps:get(path, Health, undefined),
        interval = maps:get(interval, Health, 30000),
        timeout = maps:get(timeout, Health, 5000),
        module = maps:get(module, Health, undefined)
    };
parse_health_spec(_) ->
    undefined.

%% -----------------------------------------------------------------------------
%% Internal functions - Validation
%% -----------------------------------------------------------------------------

-spec get_required(atom(), map(), atom()) -> term().
get_required(Field, Config, ExpectedType) ->
    case maps:find(Field, Config) of
        {ok, Value} ->
            validate_type(Field, Value, ExpectedType);
        error ->
            throw({missing_field, Field})
    end.

-spec validate_type(atom(), term(), atom()) -> term().
validate_type(_Field, Value, atom) when is_atom(Value) -> Value;
validate_type(_Field, Value, atom) when is_binary(Value) -> binary_to_atom(Value, utf8);
validate_type(_Field, Value, atom) when is_list(Value) -> list_to_atom(Value);
validate_type(_Field, Value, binary) when is_binary(Value) -> Value;
validate_type(_Field, Value, binary) when is_list(Value) -> list_to_binary(Value);
validate_type(_Field, Value, integer) when is_integer(Value) -> Value;
validate_type(Field, Value, ExpectedType) ->
    throw({invalid_type, Field, ExpectedType, type_of(Value)}).

-spec type_of(term()) -> atom().
type_of(V) when is_atom(V) -> atom;
type_of(V) when is_binary(V) -> binary;
type_of(V) when is_integer(V) -> integer;
type_of(V) when is_float(V) -> float;
type_of(V) when is_list(V) -> list;
type_of(V) when is_map(V) -> map;
type_of(V) when is_tuple(V) -> tuple;
type_of(_) -> unknown.
