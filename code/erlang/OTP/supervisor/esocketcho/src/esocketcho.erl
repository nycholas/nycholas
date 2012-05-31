-module(esocketcho).
-behaviour(application).
-export([start/2, stop/1]).

%% Behaviours
start(normal, _Args) ->
    esocketcho_sup:start_link().

stop(_State) ->
    ok.
