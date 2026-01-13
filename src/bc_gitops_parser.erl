%%% @doc Configuration parser for bc_gitops.
%%%
%%% This module parses application specifications from the git repository.
%%% It supports multiple configuration formats:
%%%
%%% - Erlang term files (.app.config, .config)
%%% - JSON files (.json) - requires OTP 27+ or jsx/jiffy
%%% - YAML files (.yaml, .yml) - requires yamerl dependency
%%%
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
        Description = get_optional_binary(description, Config),
        Icon = parse_icon_spec(maps:get(icon, Config, undefined), Name),
        Source = parse_source_spec(maps:get(source, Config, #{}), Version),
        Env = maps:get(env, Config, #{}),
        DependsOn = maps:get(depends_on, Config, []),
        Health = parse_health_spec(maps:get(health, Config, undefined)),
        %% VM isolation settings (v0.6.0+)
        Isolation = parse_isolation_mode(maps:get(isolation, Config, embedded)),
        VmConfig = parse_vm_config(maps:get(vm_config, Config, undefined)),

        AppSpec = #app_spec{
            name = Name,
            version = Version,
            description = Description,
            icon = Icon,
            source = Source,
            env = Env,
            depends_on = DependsOn,
            health = Health,
            isolation = Isolation,
            vm_config = VmConfig
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
%% Supports:
%% - .config, .app.config (Erlang terms)
%% - .json (JSON - requires OTP 27+ or jsx/jiffy)
%% - .yaml, .yml (YAML - requires yamerl dependency)
-spec parse_app_config_file(file:filename()) -> {ok, #app_spec{}} | {error, term()}.
parse_app_config_file(FilePath) ->
    case filename:extension(FilePath) of
        ".json" ->
            parse_json_config_file(FilePath);
        ".yaml" ->
            parse_yaml_config_file(FilePath);
        ".yml" ->
            parse_yaml_config_file(FilePath);
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
        filename:join(AppDir, "app.yaml"),
        filename:join(AppDir, "app.yml"),
        filename:join(AppDir, "app.json"),
        filename:join(AppDir, "config.yaml"),
        filename:join(AppDir, "config.yml"),
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

-spec parse_yaml_config_file(file:filename()) -> {ok, #app_spec{}} | {error, term()}.
parse_yaml_config_file(FilePath) ->
    case file:read_file(FilePath) of
        {ok, Content} ->
            case yaml_decode(Content) of
                {ok, Config} when is_map(Config) ->
                    %% Convert string keys to atoms
                    AtomConfig = atomize_keys(Config),
                    parse_app_config(AtomConfig);
                {ok, _Other} ->
                    {error, {invalid_yaml, expected_map}};
                {error, Reason} ->
                    {error, {yaml_decode_error, Reason}}
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

%% @doc Decode YAML using yamerl library.
%%
%% Requires yamerl to be available. Add to your deps:
%% {yamerl, "0.10.0"}
-spec yaml_decode(binary()) -> {ok, term()} | {error, term()}.
yaml_decode(Binary) ->
    try
        %% Check if yamerl is available
        case code:ensure_loaded(yamerl_constr) of
            {module, yamerl_constr} ->
                %% Parse YAML using yamerl
                Docs = yamerl_constr:string(binary_to_list(Binary), [
                    {detailed_constr, false},
                    {str_node_as_binary, true}
                ]),
                case Docs of
                    [Doc] ->
                        {ok, yaml_to_map(Doc)};
                    [] ->
                        {error, empty_document};
                    _Multiple ->
                        %% Take first document if multiple
                        {ok, yaml_to_map(hd(Docs))}
                end;
            {error, _} ->
                {error, {yaml_not_supported, <<"yamerl library not available. Add {yamerl, \"0.10.0\"} to deps.">>}}
        end
    catch
        throw:{yamerl_exception, Errors} ->
            {error, {yaml_parse_error, Errors}};
        error:Reason ->
            {error, Reason}
    end.

%% @doc Convert yamerl proplist output to maps recursively.
-spec yaml_to_map(term()) -> term().
yaml_to_map(List) when is_list(List) ->
    case is_proplist(List) of
        true ->
            maps:from_list([{yaml_key_to_atom(K), yaml_to_map(V)} || {K, V} <- List]);
        false ->
            [yaml_to_map(E) || E <- List]
    end;
yaml_to_map(Value) ->
    Value.

%% @doc Check if a list is a proplist (list of 2-tuples).
-spec is_proplist(list()) -> boolean().
is_proplist([]) -> true;
is_proplist([{_, _} | Rest]) -> is_proplist(Rest);
is_proplist(_) -> false.

%% @doc Convert YAML key to atom.
-spec yaml_key_to_atom(term()) -> atom().
yaml_key_to_atom(K) when is_binary(K) -> binary_to_atom(K, utf8);
yaml_key_to_atom(K) when is_list(K) -> list_to_atom(K);
yaml_key_to_atom(K) when is_atom(K) -> K.

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
    ).

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

%% @doc Parse source spec with optional version for hex packages.
%% For hex packages, if ref is not specified in source, use the app version.
-spec parse_source_spec(map(), binary() | undefined) -> #source_spec{}.
parse_source_spec(Source, AppVersion) when is_map(Source) ->
    Type = to_atom_or_default(maps:get(type, Source, hex), hex),
    %% For hex packages, use app version as ref if not explicitly set in source
    Ref = case {Type, maps:get(ref, Source, undefined)} of
        {hex, undefined} -> AppVersion;
        {_, ExplicitRef} -> ExplicitRef
    end,
    #source_spec{
        type = Type,
        url = maps:get(url, Source, undefined),
        sha256 = maps:get(sha256, Source, undefined),
        ref = Ref
    };
parse_source_spec(_, AppVersion) ->
    #source_spec{type = hex, ref = AppVersion}.

%% @doc Parse icon spec from config.
%% If undefined or identicon type, generates an identicon based on app name.
%% Supported types: url, base64, identicon (or undefined for auto-identicon)
-spec parse_icon_spec(map() | undefined, atom()) -> #icon_spec{}.
parse_icon_spec(undefined, AppName) ->
    %% No icon specified - generate identicon
    DataUri = bc_gitops_identicon:to_data_uri(AppName),
    #icon_spec{
        type = identicon,
        value = DataUri,
        mime_type = <<"image/svg+xml">>
    };
parse_icon_spec(Icon, AppName) when is_map(Icon) ->
    Type = to_atom_or_default(maps:get(type, Icon, identicon), identicon),
    case Type of
        identicon ->
            DataUri = bc_gitops_identicon:to_data_uri(AppName),
            #icon_spec{
                type = identicon,
                value = DataUri,
                mime_type = <<"image/svg+xml">>
            };
        url ->
            #icon_spec{
                type = url,
                value = to_binary_or_undefined(maps:get(value, Icon, undefined)),
                mime_type = to_binary_or_undefined(maps:get(mime_type, Icon, undefined))
            };
        base64 ->
            #icon_spec{
                type = base64,
                value = to_binary_or_undefined(maps:get(value, Icon, undefined)),
                mime_type = to_binary_or_undefined(maps:get(mime_type, Icon, <<"image/png">>))
            }
    end;
parse_icon_spec(_, AppName) ->
    %% Invalid icon config - fall back to identicon
    parse_icon_spec(undefined, AppName).

%% @doc Convert value to binary or return undefined.
-spec to_binary_or_undefined(term()) -> binary() | undefined.
to_binary_or_undefined(undefined) -> undefined;
to_binary_or_undefined(V) when is_binary(V) -> V;
to_binary_or_undefined(V) when is_list(V) -> list_to_binary(V);
to_binary_or_undefined(_) -> undefined.

%% @doc Get optional binary field from config.
-spec get_optional_binary(atom(), map()) -> binary() | undefined.
get_optional_binary(Field, Config) ->
    case maps:find(Field, Config) of
        {ok, Value} when is_binary(Value) -> Value;
        {ok, Value} when is_list(Value) -> list_to_binary(Value);
        _ -> undefined
    end.

-spec parse_health_spec(map() | undefined) -> #health_spec{} | undefined.
parse_health_spec(undefined) ->
    undefined;
parse_health_spec(Health) when is_map(Health) ->
    #health_spec{
        type = to_atom_or_default(maps:get(type, Health, http), http),
        port = maps:get(port, Health, 8080),
        path = maps:get(path, Health, undefined),
        interval = maps:get(interval, Health, 30000),
        timeout = maps:get(timeout, Health, 5000),
        module = to_atom_or_default(maps:get(module, Health, undefined), undefined)
    };
parse_health_spec(_) ->
    undefined.

%% @doc Parse isolation mode (embedded or vm).
%% Default is embedded (current behavior).
-spec parse_isolation_mode(term()) -> isolation_mode().
parse_isolation_mode(vm) -> vm;
parse_isolation_mode(<<"vm">>) -> vm;
parse_isolation_mode("vm") -> vm;
parse_isolation_mode(embedded) -> embedded;
parse_isolation_mode(<<"embedded">>) -> embedded;
parse_isolation_mode("embedded") -> embedded;
parse_isolation_mode(_) -> embedded.  %% Default to embedded for backwards compat

%% @doc Parse VM configuration for isolated deployments.
-spec parse_vm_config(map() | undefined) -> #vm_config{} | undefined.
parse_vm_config(undefined) ->
    undefined;
parse_vm_config(VmConfig) when is_map(VmConfig) ->
    #vm_config{
        memory_limit = maps:get(memory_limit, VmConfig, undefined),
        scheduler_limit = maps:get(scheduler_limit, VmConfig, undefined),
        node_prefix = to_binary_or_undefined(maps:get(node_prefix, VmConfig, undefined)),
        extra_args = parse_extra_args(maps:get(extra_args, VmConfig, []))
    };
parse_vm_config(_) ->
    undefined.

%% @doc Parse extra VM arguments to list of binaries.
-spec parse_extra_args(term()) -> [binary()].
parse_extra_args(Args) when is_list(Args) ->
    [to_binary_or_undefined(A) || A <- Args, A =/= undefined];
parse_extra_args(_) ->
    [].

%% @doc Convert value to atom, or return default if undefined/conversion fails.
-spec to_atom_or_default(term(), atom()) -> atom().
to_atom_or_default(undefined, Default) -> Default;
to_atom_or_default(V, _Default) when is_atom(V) -> V;
to_atom_or_default(V, _Default) when is_binary(V) -> binary_to_atom(V, utf8);
to_atom_or_default(V, _Default) when is_list(V) -> list_to_atom(V);
to_atom_or_default(_, Default) -> Default.

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

-spec validate_type(atom(), term(), atom | binary) -> term().
validate_type(_Field, Value, atom) when is_atom(Value) -> Value;
validate_type(_Field, Value, atom) when is_binary(Value) -> binary_to_atom(Value, utf8);
validate_type(_Field, Value, atom) when is_list(Value) -> list_to_atom(Value);
validate_type(_Field, Value, binary) when is_binary(Value) -> Value;
validate_type(_Field, Value, binary) when is_list(Value) -> list_to_binary(Value);
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
