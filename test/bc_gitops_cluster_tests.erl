%%% @doc Unit tests for bc_gitops_cluster module.
%%%
%%% Tests cluster management, cookie resolution, node naming,
%%% and macula integration detection.
%%% @end
-module(bc_gitops_cluster_tests).

-include_lib("eunit/include/eunit.hrl").
-include("bc_gitops.hrl").

%% -----------------------------------------------------------------------------
%% Macula Detection Tests
%% -----------------------------------------------------------------------------

macula_available_returns_boolean_test() ->
    %% macula module may or may not be available depending on test environment
    Result = bc_gitops_cluster:macula_available(),
    ?assert(is_boolean(Result)).

%% -----------------------------------------------------------------------------
%% Node Name Generation Tests
%% -----------------------------------------------------------------------------

generate_node_name_without_config_test() ->
    NodeName = bc_gitops_cluster:generate_node_name(test_app, undefined),
    ?assert(is_atom(NodeName)),
    %% Should be test_app@hostname
    NodeStr = atom_to_list(NodeName),
    ?assert(lists:prefix("test_app@", NodeStr)).

generate_node_name_with_empty_prefix_test() ->
    Config = #vm_config{
        node_prefix = undefined,
        extra_args = []
    },
    NodeName = bc_gitops_cluster:generate_node_name(my_app, Config),
    NodeStr = atom_to_list(NodeName),
    ?assert(lists:prefix("my_app@", NodeStr)).

generate_node_name_with_prefix_test() ->
    Config = #vm_config{
        node_prefix = <<"guest_">>,
        extra_args = []
    },
    NodeName = bc_gitops_cluster:generate_node_name(my_app, Config),
    NodeStr = atom_to_list(NodeName),
    ?assert(lists:prefix("guest_my_app@", NodeStr)).

generate_node_name_uses_hostname_test() ->
    {ok, ExpectedHost} = inet:gethostname(),
    NodeName = bc_gitops_cluster:generate_node_name(test_app, undefined),
    NodeStr = atom_to_list(NodeName),
    %% Should end with @hostname
    ?assert(lists:suffix("@" ++ ExpectedHost, NodeStr)).

%% -----------------------------------------------------------------------------
%% Hostname Tests
%% -----------------------------------------------------------------------------

get_hostname_returns_string_test() ->
    Hostname = bc_gitops_cluster:get_hostname(),
    ?assert(is_list(Hostname)),
    ?assert(length(Hostname) > 0).

get_hostname_matches_inet_test() ->
    {ok, Expected} = inet:gethostname(),
    Actual = bc_gitops_cluster:get_hostname(),
    ?assertEqual(Expected, Actual).

%% -----------------------------------------------------------------------------
%% Cookie Resolution Tests
%% -----------------------------------------------------------------------------

resolve_cookie_returns_valid_result_test() ->
    %% resolve_cookie should return {ok, atom()} or {error, not_found}
    Result = bc_gitops_cluster:resolve_cookie(),
    case Result of
        {ok, Cookie} ->
            ?assert(is_atom(Cookie));
        {error, not_found} ->
            ok
    end.

read_cookie_file_returns_valid_result_test() ->
    %% read_cookie_file should return {ok, atom()} or {error, _}
    Result = bc_gitops_cluster:read_cookie_file(),
    case Result of
        {ok, Cookie} ->
            ?assert(is_atom(Cookie));
        {error, _Reason} ->
            ok
    end.

get_cookie_returns_atom_test() ->
    %% get_cookie should always return an atom (generating one if needed)
    Cookie = bc_gitops_cluster:get_cookie(),
    ?assert(is_atom(Cookie)).

%% -----------------------------------------------------------------------------
%% Node Connection Tests
%% -----------------------------------------------------------------------------

is_node_connected_self_test() ->
    %% Our own node shouldn't be in nodes() list
    %% (nodes() returns other nodes, not self)
    Self = node(),
    ?assertNot(bc_gitops_cluster:is_node_connected(Self)).

is_node_connected_nonexistent_test() ->
    %% A fake node should not be connected
    FakeNode = 'fake_node_12345@nonexistent',
    ?assertNot(bc_gitops_cluster:is_node_connected(FakeNode)).

%% -----------------------------------------------------------------------------
%% Wait for Node Tests
%% -----------------------------------------------------------------------------

wait_for_node_timeout_test() ->
    %% Waiting for a non-existent node should timeout
    FakeNode = 'fake_node_12345@nonexistent',
    %% Use short timeout to avoid slow test
    Result = bc_gitops_cluster:wait_for_node(FakeNode, 100),
    ?assertEqual({error, timeout}, Result).

%% -----------------------------------------------------------------------------
%% RPC Check App Tests
%% -----------------------------------------------------------------------------

rpc_check_app_self_kernel_test() ->
    %% If we're running distributed, check that kernel is running on self
    case node() of
        nonode@nohost ->
            %% Not distributed, skip test
            ok;
        SelfNode ->
            Result = bc_gitops_cluster:rpc_check_app(SelfNode, kernel),
            ?assertEqual({ok, running}, Result)
    end.

rpc_check_app_self_nonexistent_test() ->
    %% Check for a non-existent app on self
    case node() of
        nonode@nohost ->
            ok;
        SelfNode ->
            Result = bc_gitops_cluster:rpc_check_app(SelfNode, nonexistent_app_12345),
            ?assertEqual({ok, not_running}, Result)
    end.

rpc_check_app_remote_failure_test() ->
    %% RPC to non-existent node should fail
    FakeNode = 'fake_node_12345@nonexistent',
    Result = bc_gitops_cluster:rpc_check_app(FakeNode, kernel),
    ?assertMatch({error, {rpc_failed, _}}, Result).

%% -----------------------------------------------------------------------------
%% Distribution Tests
%% -----------------------------------------------------------------------------

ensure_distributed_idempotent_test() ->
    %% ensure_distributed should be safe to call multiple times
    %% and should return ok
    Result1 = bc_gitops_cluster:ensure_distributed(),
    Result2 = bc_gitops_cluster:ensure_distributed(),
    %% Both should succeed (ok) or fail with same error
    ?assertEqual(Result1, Result2).

%% -----------------------------------------------------------------------------
%% Monitor Nodes Tests
%% -----------------------------------------------------------------------------

monitor_nodes_succeeds_test() ->
    %% Monitor nodes should succeed (may need distribution)
    case bc_gitops_cluster:ensure_distributed() of
        ok ->
            Result = bc_gitops_cluster:monitor_nodes(),
            ?assertEqual(ok, Result),
            %% Clean up
            bc_gitops_cluster:unmonitor_nodes();
        {error, _} ->
            %% Distribution not available, skip
            ok
    end.

unmonitor_nodes_succeeds_test() ->
    %% Unmonitor should succeed even if not monitoring
    case bc_gitops_cluster:ensure_distributed() of
        ok ->
            Result = bc_gitops_cluster:unmonitor_nodes(),
            ?assertEqual(ok, Result);
        {error, _} ->
            ok
    end.
