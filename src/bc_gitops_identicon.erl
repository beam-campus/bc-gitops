%%% @doc Identicon generator for bc_gitops applications.
%%%
%%% Generates unique, deterministic SVG identicons based on app names.
%%% The algorithm creates a 5x5 symmetric grid pattern with colors
%%% derived from a hash of the input.
%%%
%%% == Usage ==
%%%
%%% ```
%%% %% Generate SVG as binary
%%% Svg = bc_gitops_identicon:generate(demo_uptime).
%%%
%%% %% Generate as data URI for embedding in HTML
%%% DataUri = bc_gitops_identicon:to_data_uri(demo_uptime).
%%% '''
%%%
%%% @end
-module(bc_gitops_identicon).

%% API
-export([
    generate/1,
    generate/2,
    to_data_uri/1,
    to_data_uri/2
]).

-define(DEFAULT_SIZE, 64).
-define(GRID_SIZE, 5).

%% -----------------------------------------------------------------------------
%% API
%% -----------------------------------------------------------------------------

%% @doc Generate an SVG identicon for the given input.
%% Input can be an atom, binary, or string.
-spec generate(atom() | binary() | string()) -> binary().
generate(Input) ->
    generate(Input, ?DEFAULT_SIZE).

%% @doc Generate an SVG identicon with custom size.
-spec generate(atom() | binary() | string(), pos_integer()) -> binary().
generate(Input, Size) when is_atom(Input) ->
    generate(atom_to_binary(Input, utf8), Size);
generate(Input, Size) when is_list(Input) ->
    generate(list_to_binary(Input), Size);
generate(Input, Size) when is_binary(Input) ->
    Hash = crypto:hash(md5, Input),
    {Hue, Saturation, Lightness} = extract_color(Hash),
    Grid = extract_grid(Hash),
    render_svg(Grid, Hue, Saturation, Lightness, Size).

%% @doc Generate identicon as a data URI for HTML embedding.
-spec to_data_uri(atom() | binary() | string()) -> binary().
to_data_uri(Input) ->
    to_data_uri(Input, ?DEFAULT_SIZE).

%% @doc Generate identicon as a data URI with custom size.
-spec to_data_uri(atom() | binary() | string(), pos_integer()) -> binary().
to_data_uri(Input, Size) ->
    Svg = generate(Input, Size),
    Base64 = base64:encode(Svg),
    <<"data:image/svg+xml;base64,", Base64/binary>>.

%% -----------------------------------------------------------------------------
%% Internal - Color extraction
%% -----------------------------------------------------------------------------

%% @doc Extract HSL color from hash bytes.
%% Uses first 3 bytes to determine hue, saturation, and lightness.
-spec extract_color(binary()) -> {non_neg_integer(), non_neg_integer(), non_neg_integer()}.
extract_color(<<H:8, S:8, L:8, _/binary>>) ->
    %% Hue: 0-360 degrees
    Hue = H * 360 div 256,
    %% Saturation: 50-80% (avoid too gray or too saturated)
    Saturation = 50 + (S * 30 div 256),
    %% Lightness: 40-60% (avoid too dark or too light)
    Lightness = 40 + (L * 20 div 256),
    {Hue, Saturation, Lightness}.

%% -----------------------------------------------------------------------------
%% Internal - Grid extraction
%% -----------------------------------------------------------------------------

%% @doc Extract a 5x5 symmetric grid pattern from hash.
%% Only the left half + center column is derived from hash,
%% then mirrored to create vertical symmetry.
-spec extract_grid(binary()) -> [[boolean()]].
extract_grid(<<_:3/binary, GridBytes:8/binary, _/binary>>) ->
    %% We need 15 bits for a 5x5 symmetric grid (3 cols * 5 rows)
    %% Using 8 bytes gives us plenty of entropy
    Bits = extract_bits(GridBytes),
    build_symmetric_grid(Bits).

%% @doc Extract bits from bytes as a list of booleans.
-spec extract_bits(binary()) -> [boolean()].
extract_bits(Bytes) ->
    lists:flatten([byte_to_bits(B) || <<B:8>> <= Bytes]).

-spec byte_to_bits(byte()) -> [boolean()].
byte_to_bits(Byte) ->
    [(Byte band (1 bsl I)) =/= 0 || I <- lists:seq(7, 0, -1)].

%% @doc Build a 5x5 grid with vertical symmetry.
%% Takes first 15 bits for left-half + center, mirrors to right.
-spec build_symmetric_grid([boolean()]) -> [[boolean()]].
build_symmetric_grid(Bits) ->
    %% Take 15 bits (3 columns x 5 rows)
    {GridBits, _} = lists:split(15, Bits),

    %% Split into 5 rows of 3 bits each
    Rows = split_into_rows(GridBits, 3),

    %% Mirror each row: [A, B, C] -> [A, B, C, B, A]
    [mirror_row(Row) || Row <- Rows].

-spec split_into_rows([boolean()], pos_integer()) -> [[boolean()]].
split_into_rows([], _RowSize) -> [];
split_into_rows(Bits, RowSize) ->
    {Row, Rest} = lists:split(RowSize, Bits),
    [Row | split_into_rows(Rest, RowSize)].

-spec mirror_row([boolean()]) -> [boolean()].
mirror_row([A, B, C]) ->
    [A, B, C, B, A].

%% -----------------------------------------------------------------------------
%% Internal - SVG rendering
%% -----------------------------------------------------------------------------

%% @doc Render the grid as an SVG.
-spec render_svg([[boolean()]], non_neg_integer(), non_neg_integer(), non_neg_integer(), pos_integer()) -> binary().
render_svg(Grid, Hue, Saturation, Lightness, Size) ->
    CellSize = Size div ?GRID_SIZE,
    ActualSize = CellSize * ?GRID_SIZE,

    %% Generate HSL color string
    Color = io_lib:format("hsl(~B, ~B%, ~B%)", [Hue, Saturation, Lightness]),

    %% Background color (lighter version)
    BgLightness = min(95, Lightness + 40),
    BgColor = io_lib:format("hsl(~B, ~B%, ~B%)", [Hue, Saturation div 2, BgLightness]),

    %% Generate rectangles for filled cells
    Rects = render_cells(Grid, CellSize, Color),

    %% Assemble SVG
    Svg = io_lib:format(
        "<svg xmlns=\"http://www.w3.org/2000/svg\" "
        "width=\"~B\" height=\"~B\" viewBox=\"0 0 ~B ~B\">"
        "<rect width=\"100%\" height=\"100%\" fill=\"~s\" rx=\"~B\"/>"
        "~s"
        "</svg>",
        [ActualSize, ActualSize, ActualSize, ActualSize, BgColor, CellSize div 4, Rects]
    ),
    iolist_to_binary(Svg).

%% @doc Render all filled cells as SVG rectangles.
-spec render_cells([[boolean()]], pos_integer(), iolist()) -> iolist().
render_cells(Grid, CellSize, Color) ->
    {_, Rects} = lists:foldl(
        fun(Row, {Y, Acc}) ->
            RowRects = render_row(Row, Y, CellSize, Color),
            {Y + 1, [Acc, RowRects]}
        end,
        {0, []},
        Grid
    ),
    Rects.

-spec render_row([boolean()], non_neg_integer(), pos_integer(), iolist()) -> iolist().
render_row(Row, Y, CellSize, Color) ->
    {_, Rects} = lists:foldl(
        fun(Filled, {X, Acc}) ->
            Rect = case Filled of
                true ->
                    Margin = CellSize div 8,
                    RectSize = CellSize - (2 * Margin),
                    io_lib:format(
                        "<rect x=\"~B\" y=\"~B\" width=\"~B\" height=\"~B\" "
                        "fill=\"~s\" rx=\"~B\"/>",
                        [X * CellSize + Margin, Y * CellSize + Margin,
                         RectSize, RectSize, Color, Margin]
                    );
                false ->
                    []
            end,
            {X + 1, [Acc, Rect]}
        end,
        {0, []},
        Row
    ),
    Rects.
