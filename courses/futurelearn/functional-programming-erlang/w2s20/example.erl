-module(example).

-export([take/2, tests/0]).

-spec take(integer(), [T]) -> [T].
take(0, _) ->
    [];
take(_, []) ->
    [];
take(N, L) when N > 0 ->
    take(N, L, []).

-spec take(integer(), [T], [T]) -> [T].
take(0, _, Acc) ->
    lists_reverse(Acc);
take(_, [], Acc) ->
    lists_reverse(Acc);
take(N, [E | L], Acc) ->
    take(N - 1, L, [E | Acc]).

-spec tests() -> ok.
tests() ->
    [] = take(0, "hello"),
    [] = take(10, []),
    "hell" = take(4, "hello"),
    "hello" = take(5, "hello"),
    "hello" = take(9, "hello"),
    ok.

-spec lists_reverse([T]) -> [T].
lists_reverse(L) ->
    lists_reverse(L, []).

-spec lists_reverse([T], [T]) -> [T].
lists_reverse([], Acc) ->
    Acc;
lists_reverse([E | L], Acc) ->
    lists_reverse(L, [E | Acc]).
    