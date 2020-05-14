-module(list).

-export([product/1, producttr/1, maximum/1, maximumtr/1, tests/0]).

product([]) -> 
    1;
product([N|L]) -> 
    N * product(L).

producttr(L) when is_list(L) ->
    producttr(L, 1).

producttr([], Acc) ->
    Acc;
producttr([N|L], Acc) ->
    producttr(L, Acc * N).

maximum([]) ->
    undefined;
maximum([N]) ->
    N;
maximum([N, M|[]]) ->
    max(N, M);
maximum([N, M|L]) ->
    max(max(N, M), maximum(L)).

maximumtr([]) ->
        undefined;
maximumtr(L = [N|_]) ->
    maximumtr(L, N).

maximumtr([], Max) -> 
    Max;
maximumtr([N|L], Max) when is_list(L) -> 
    maximumtr(L, max(Max, N)).

tests() ->
    %% product
    1 = product([]),
    -1 = product([-1]),
    6 = product([2, 3]),
    %% producttr
    1 = producttr([]),
    -1 = producttr([-1]),
    6 = producttr([2, 3]),
    %% maximum
    undefined = maximum([]),
    4 = maximum([1, 2, -1, 4, 2, -1]),
    4 = maximum([1, 2, -1, 4, 2, -1, 3]),
    -1 = maximum([-1]),
    %% maximumtr
    undefined = maximumtr([]),
    4 = maximumtr([1, 2, -1, 4, 2, -1]),
    4 = maximumtr([1, 2, -1, 4, 2, -1, 3]),
    -1 = maximumtr([-1]),
    ok.