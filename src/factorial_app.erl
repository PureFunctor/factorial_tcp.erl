%%%-------------------------------------------------------------------
%% @doc factorial public API
%% @end
%%%-------------------------------------------------------------------

-module(factorial_app).

-behaviour(application).

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
    factorial_sup:start_link().

stop(_State) ->
    ok.

%% internal functions
