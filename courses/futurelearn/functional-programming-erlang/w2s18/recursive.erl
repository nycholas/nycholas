% Copyright (c) 2020, Nycholas de Oliveira e Oliveira <nycholas@gmail.com>
% All rights reserved.
%
% Redistribution and use in source and binary forms, with or without
% modification, are permitted provided that the following conditions are met:
%
% % Redistributions of source code must retain the above copyright notice,
%    this list of conditions and the following disclaimer.
% % Redistributions in binary form must reproduce the above copyright notice,
%    this list of conditions and the following disclaimer in the documentation
%    and/or other materials provided with the distribution.
% % Neither the name of the Nycholas de Oliveira e Oliveira nor the names of
%    its contributors may be used to endorse or promote products derived from
%    this software without specific prior written permission.
%
% THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS"
% AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
% IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
% ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE
% LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR
% CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF
% SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS
% INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN
% CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE)
% ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
% POSSIBILITY OF SUCH DAMAGE.
-module(recursive).

-export([double/1, evens/1, median/1, modes/1, tests/0]).

double([]) ->
    [];
double([N | L]) ->
    [2 * N | double(L)].

evens([]) ->
    [];
evens([N | L]) ->
    case N rem 2 of
        0 -> 
            [N | evens(L)];
        _ ->
            evens(L)
    end.

median([]) ->
    [];
median(L) when is_list(L) ->
    LSort = lists_sort(L),
    Len = length(LSort),
    {L1, L2} = lists_split(Len div 2, LSort),
    case Len rem 2 of
        1 ->
            [E | _] = L2,
            [E];
        _ ->
            E1 = lists_last(L1),
            [E2 | _] = L2,
            [E1, E2]
    end.

modes([]) ->
    [];
modes(L) ->
    LUSort = lists_usort(L),
    LCount = [{N, lists_count(N, L)} || N <- LUSort],
    MaxCount = lists_max([X || {_, X} <- LCount]),
    [N || {N, C} <- LCount, C == MaxCount].

tests() ->
    %% Lists
    [3, 2, 1] = lists_reverse([1, 2, 3]),
    [1, 2, 3, 4, 5, 6] = lists_merge([1, 2, 3], [4, 5, 6]),
    [1, 2, 3, -2, 4, 5, 6, -1] = lists_merge([1, 2, 3, -2], [4, 5, 6, -1]),
    3 = lists_count(1, [1, 2, 3, 1, 2, 1]),
    5 = lists_last([1, 2, 3, 4, 5]),
    {[1, 2], [3, 4, 5]} = lists_split(2, [1, 2, 3, 4, 5]),
    5 = lists_max([1, 4, 3, 5, 2, 1, 0]),
    [-1, 1, 2, 2, 3, 4, 4, 6, 7, 8] = lists_sort([4, 2, 4, 6, 7, 8, 1, 2, -1, 3]),
    [-1, 1, 2, 3, 4, 6, 7, 8] = lists_usort([4, 2, 4, 6, 7, 8, 1, 2, -1, 3]),

    %% Double
    [] = double([]),
    [2] = double([1]),
    [4] = double([2]),
    [2, 4, 6, 8] = double([1, 2, 3, 4]),
    [2, 4, 6, 8, 10] = double([1, 2, 3, 4, 5]),

    %% Evens
    [] = evens([]),
    [] = evens([1]),
    [] = evens([1, 3, 5, 7]),
    [2] = evens([2]),
    [2, 4, 6, 8] = evens([1, 2, 3, 4, 5, 6, 7, 8]),
    [2, 4, 6, 8, 10] = evens([1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11]),

    %% Median
    [] = median([]),
    [1] = median([1]),
    [1, 2] = median([1, 2]),
    [2] = median([1, 2, 3]),
    [2] = median([1, 3, 2]),
    [3] = median([1, 3, 2, 4, 5]),
    
    %% Modes
    [] = modes([]),
    [1, 2, 3, 4, 5] = modes([1, 2, 3, 4, 5]),
    [1] = modes([1, 1, 1, 2, 2, 3, 4, 1, 1, 1]),
    [1, 2, 3] = modes([1, 1, 2, 2, 3, 3]),
    ok.

lists_usort(L) ->
    [E1 | L2] = lists_sort(L),
    lists_reverse(lists_usort(E1, L2, [])).

lists_usort(E, [], Acc) ->
    [E | Acc];
lists_usort(E, [E | L], Acc) ->
    lists_usort(E, L, Acc);
lists_usort(E1, [E2 | L], Acc) ->
    lists_usort(E2, L, [E1 | Acc]).

lists_sort([]) ->
    [];
lists_sort([E | L]) ->
    {Smaller, Larger} = lists_partition(E, L, [], []),
    L1 = lists_merge(lists_sort(Smaller), [E]),
    lists_merge(L1, lists_sort(Larger)).

lists_partition(_, [], Smaller, Larger) -> 
    {Smaller, Larger};
lists_partition(E1, [E2 | L], Smaller, Larger) -> 
    case E1 >= E2 of
        true ->
            lists_partition(E1, L, [E2 | Smaller], Larger);
        _ ->
            lists_partition(E1, L, Smaller, [E2 | Larger])
    end.

lists_max([E]) ->
    E;
lists_max([E | L]) ->
    lists_max(L, E).

lists_max([], Max) ->
    Max;
lists_max([E | L], Max) ->
    lists_max(L, max(Max, E)).

lists_split(0, L) ->
    {[], L};
lists_split(N, L) ->
    lists_split(N, [], L).

lists_split(0, L1, L2) ->
    {lists_reverse(L1), L2};
lists_split(N, L1, [E | L2]) ->
    lists_split(N - 1, [E | L1], L2).

lists_last([E]) -> E;
lists_last([_ | L]) ->
    lists_last(L).

lists_count(_, []) -> 
    0;
lists_count(E, [E | L]) -> 
    1 + lists_count(E, L);
lists_count(E, [_ | L]) -> 
    lists_count(E, L).

lists_reverse(L) ->
    lists_reverse(L, []).

lists_reverse([], Acc) ->
    Acc;
lists_reverse([E | L], Acc) ->
    lists_reverse(L, [E | Acc]).

lists_merge(L1, []) ->
    L1;
lists_merge(L1, L2) ->
    L3 = lists_reverse(L1),
    lists_merge(L3, L2, L2).

lists_merge([], _, Acc) ->
    Acc;
lists_merge([E | L1], L2, Acc) ->
    lists_merge(L1, L2, [E | Acc]).
