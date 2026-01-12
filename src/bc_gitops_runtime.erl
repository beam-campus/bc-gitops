%%% @doc Runtime behaviour for bc_gitops.
%%%
%%% This behaviour defines the interface that runtime modules must
%%% implement to handle application lifecycle operations.
%%%
%%% == Implementing a Runtime ==
%%%
%%% ```
%%% -module(my_runtime).
%%% -behaviour(bc_gitops_runtime).
%%%
%%% -export([deploy/1, remove/1, upgrade/2, reconfigure/1, get_current_state/0]).
%%%
%%% deploy(AppSpec) ->
%%%     %% Download and start the application
%%%     {ok, AppState}.
%%%
%%% remove(AppName) ->
%%%     %% Stop and remove the application
%%%     ok.
%%%
%%% upgrade(AppSpec, OldVersion) ->
%%%     %% Perform hot upgrade or restart with new version
%%%     {ok, AppState}.
%%%
%%% reconfigure(AppSpec) ->
%%%     %% Update application environment without restart
%%%     {ok, AppState}.
%%%
%%% get_current_state() ->
%%%     %% Return map of currently running apps
%%%     {ok, #{}}.
%%% '''
%%%
%%% == Deployment Strategies ==
%%%
%%% The runtime can implement different deployment strategies:
%%%
%%% <ul>
%%% <li><b>Hot code upgrade</b> - Use OTP release_handler for zero-downtime upgrades</li>
%%% <li><b>Rolling restart</b> - Stop old version, start new version</li>
%%% <li><b>Blue-green</b> - Start new version alongside old, then switch</li>
%%% </ul>
%%%
%%% @end
-module(bc_gitops_runtime).

-include("bc_gitops.hrl").

%% Behaviour callbacks
-callback deploy(AppSpec :: #app_spec{}) ->
    {ok, #app_state{}} | {error, term()}.

-callback remove(AppName :: atom()) ->
    ok | {error, term()}.

-callback upgrade(AppSpec :: #app_spec{}, OldVersion :: binary()) ->
    {ok, #app_state{}} | {error, term()}.

-callback reconfigure(AppSpec :: #app_spec{}) ->
    {ok, #app_state{}} | {error, term()}.

%% Optional callback
-callback get_current_state() ->
    {ok, #{atom() => #app_state{}}} | {error, term()}.

-optional_callbacks([get_current_state/0]).
