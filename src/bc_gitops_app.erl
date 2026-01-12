%%% @doc Application callback module for bc_gitops.
%%%
%%% This module implements the OTP application behaviour, starting
%%% the supervision tree when the application starts.
%%% @end
-module(bc_gitops_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

%% -----------------------------------------------------------------------------
%% Application callbacks
%% -----------------------------------------------------------------------------

-spec start(application:start_type(), term()) -> {ok, pid()} | {error, term()}.
start(_StartType, _StartArgs) ->
    bc_gitops_sup:start_link().

-spec stop(term()) -> ok.
stop(_State) ->
    ok.
