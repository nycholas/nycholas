%%%-------------------------------------------------------------------
%% @doc calc public API
%% @end
%%%-------------------------------------------------------------------

-module(calc_app).

-behaviour(application).

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
    calc_sup:start_link().

stop(_State) ->
    ok.

%% internal functions
