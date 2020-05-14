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
    LSort = lists:sort(L),
    Len = length(LSort),
    {L1, L2} = lists:split(Len div 2, LSort),
    case Len rem 2 of
        1 ->
            [E | _] = L2,
            [E];
        _ ->
            E1 = lists:last(L1),
            [E2 | _] = L2,
            [E1, E2]
    end.

modes([]) ->
    [];
modes(L) ->
    LUSort = lists:usort(L),
    LCount = [{N, count(N, L)} || N <- LUSort],
    MaxCount = lists:max([X || {_, X} <- LCount]),
    [N || {N, C} <- LCount, C == MaxCount].

tests() ->
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

count(_, []) -> 
    0;
count(E, [E|L]) -> 
    1 + count(E, L);
count(E, [_|L]) -> 
    count(E, L).
