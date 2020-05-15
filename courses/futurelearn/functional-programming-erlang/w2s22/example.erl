-module(example).

-export([nub/1, tests/0]).

-spec nub([T]) -> [T].
nub([]) -> 
    [];
nub(L) -> 
    nub(L, []).

-spec nub([T], [T]) -> [T].
nub([], Acc) ->
    lists_reverse(Acc);
nub([E | L], Acc) ->
    L1 = lists_delete(E, L),
    nub(L1, [E | Acc]).

-spec tests() -> ok.
tests() ->
    [] = lists_delete(0, []),
    [1, 2, 3, 5] = lists_delete(4, [1, 2, 3, 4, 5]), 
    [1, 2, 3, 3, 5] = lists_delete(4, [1, 2, 3, 3, 4, 5, 4, 4]), 
    [] = nub([]),
    [2, 4, 1, 3] = nub([2, 4, 1, 3, 3, 1]),
    ok.

-spec lists_delete(T, [T]) -> [T].
lists_delete(_, []) ->
    [];
lists_delete(E, L) ->
    lists_delete(E, L, []).

-spec lists_delete(T, [T], [T]) -> [T].
lists_delete(_, [], Acc) ->
    lists_reverse(Acc);
lists_delete(E, [E | L], Acc) ->
    lists_delete(E, L, Acc);
lists_delete(E1, [E2 | L], Acc) ->
    lists_delete(E1, L, [E2 | Acc]).

-spec lists_reverse([T]) -> [T].
lists_reverse(L) ->
    lists_reverse(L, []).

-spec lists_reverse([T], [T]) -> [T].
lists_reverse([], Acc) ->
    Acc;
lists_reverse([E | L], Acc) ->
    lists_reverse(L, [E | Acc]).
    
