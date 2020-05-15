-module(example).

-export([take/2, tests/0]).

take(0, _) ->
    [];
take(N, L) when N > 0 ->
    take(N, L, []).

take(0, _, Acc) ->
    lists_reverse(Acc);
take(_, [], Acc) ->
    lists_reverse(Acc);
take(N, [E | L], Acc) ->
    take(N - 1, L, [E | Acc]).

tests() ->
    [] = take(0, "hello"),
    "hell" = take(4, "hello"),
    "hello" = take(5, "hello"),
    "hello" = take(9, "hello"),
    ok.

lists_reverse(L) ->
    lists_reverse(L, []).

lists_reverse([], Acc) ->
    Acc;
lists_reverse([E | L], Acc) ->
    lists_reverse(L, [E | Acc]).
    