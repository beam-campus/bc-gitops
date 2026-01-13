-module(bc_gitops_identicon_tests).

-include_lib("eunit/include/eunit.hrl").

%% -----------------------------------------------------------------------------
%% Test generators
%% -----------------------------------------------------------------------------

identicon_test_() ->
    [
        {"generate/1 returns SVG binary", fun generate_returns_svg/0},
        {"generate/1 is deterministic", fun generate_is_deterministic/0},
        {"generate/1 handles atoms", fun generate_handles_atoms/0},
        {"generate/1 handles binaries", fun generate_handles_binaries/0},
        {"generate/1 handles strings", fun generate_handles_strings/0},
        {"generate/2 respects size parameter", fun generate_respects_size/0},
        {"to_data_uri/1 returns data URI", fun to_data_uri_returns_data_uri/0},
        {"different inputs produce different icons", fun different_inputs_different_icons/0}
    ].

%% -----------------------------------------------------------------------------
%% Test cases
%% -----------------------------------------------------------------------------

generate_returns_svg() ->
    Result = bc_gitops_identicon:generate(test_app),
    ?assert(is_binary(Result)),
    %% Should start with SVG tag
    ?assertMatch(<<"<svg", _/binary>>, Result),
    %% Should contain closing tag
    ?assertNotEqual(nomatch, binary:match(Result, <<"</svg>">>)).

generate_is_deterministic() ->
    Result1 = bc_gitops_identicon:generate(demo_uptime),
    Result2 = bc_gitops_identicon:generate(demo_uptime),
    ?assertEqual(Result1, Result2).

generate_handles_atoms() ->
    Result = bc_gitops_identicon:generate(my_app),
    ?assert(is_binary(Result)),
    ?assertMatch(<<"<svg", _/binary>>, Result).

generate_handles_binaries() ->
    Result = bc_gitops_identicon:generate(<<"my_app">>),
    ?assert(is_binary(Result)),
    ?assertMatch(<<"<svg", _/binary>>, Result).

generate_handles_strings() ->
    Result = bc_gitops_identicon:generate("my_app"),
    ?assert(is_binary(Result)),
    ?assertMatch(<<"<svg", _/binary>>, Result).

generate_respects_size() ->
    Small = bc_gitops_identicon:generate(test, 32),
    Large = bc_gitops_identicon:generate(test, 128),
    %% Larger size should produce longer SVG (more viewBox/width/height values)
    ?assert(byte_size(Large) >= byte_size(Small)),
    %% Check size attributes are present
    ?assertNotEqual(nomatch, binary:match(Small, <<"width=\"">>)),
    ?assertNotEqual(nomatch, binary:match(Large, <<"width=\"">>)).

to_data_uri_returns_data_uri() ->
    Result = bc_gitops_identicon:to_data_uri(test_app),
    ?assert(is_binary(Result)),
    %% Should start with data URI scheme
    ?assertMatch(<<"data:image/svg+xml;base64,", _/binary>>, Result).

different_inputs_different_icons() ->
    Icon1 = bc_gitops_identicon:generate(app_one),
    Icon2 = bc_gitops_identicon:generate(app_two),
    Icon3 = bc_gitops_identicon:generate(app_three),
    %% All should be different
    ?assertNotEqual(Icon1, Icon2),
    ?assertNotEqual(Icon2, Icon3),
    ?assertNotEqual(Icon1, Icon3).
