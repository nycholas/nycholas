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
-module(hofs).

-export([double/1, evens/1, product/1, zip_v1/2, zip_v2/2, zip_with_v1/3, zip_with_v2/3]).

 -include_lib("eunit/include/eunit.hrl").

-spec double([integer()]) -> [integer()].
double(List) ->
    lists:map(fun(N) -> N * 2 end, List).

-spec evens([integer()]) -> [integer()].
evens(List) -> 
    lists:filter(fun(N) -> N rem 2 == 0 end, List).

-spec product([integer()]) -> [integer()].
product(List) ->
    lists:foldr(fun(N, Acc) -> N * Acc end, 1, List).

-spec zip_v1([integer()], [integer()]) -> [{integer()}].
zip_v1([], _List2) ->
    []; 
zip_v1(_List1, []) ->
    []; 
zip_v1([E1 | List1], [E2 | List2]) ->
    [{E1, E2} | zip_v1(List1, List2)].

-spec zip_v2([integer()], [integer()]) -> [{integer()}].
zip_v2(List1, List2) ->
    zip_with_v2(fun(E1, E2) -> {E1, E2} end, List1, List2).

-spec zip_with_v1(function(), [integer()], [integer()]) -> [{integer()}].
zip_with_v1(_Fn, [], _List2) -> 
    [];
zip_with_v1(_Fn, _List1, []) -> 
    [];
zip_with_v1(Fn, [E1 | List1], [E2 | List2]) -> 
    [Fn(E1, E2) | zip_with_v1(Fn, List1, List2)].

-spec zip_with_v2(function(), [integer()], [integer()]) -> [{integer()}].
zip_with_v2(Fn, List1, List2) when length(List1) > length(List2) ->
    {List3, _} = lists:split(length(List2), List1),
    zip_with_v2(Fn, List3, List2);
zip_with_v2(Fn, List1, List2) when length(List1) < length(List2) ->
    {List3, _} = lists:split(length(List1), List2),
    zip_with_v2(Fn, List1, List3);
zip_with_v2(Fn, List1, List2) ->
    lists:map(fun({E1, E2}) -> Fn(E1, E2) end, lists:zip(List1, List2)).

%%====================================================================
%% Unit Tests
%%====================================================================

-spec double_test() -> ok.
double_test() -> 
    [] = double([]),
    [2] = double([1]),
    [2, 4, 6, 8] = double([1, 2, 3, 4]),
    ok.

-spec evens_test() -> ok.
evens_test() -> 
    [] = evens([]),
    [] = evens([1]),
    [2] = evens([2]),
    [2, 4, 6, 8] = evens([1, 2, 3, 4, 5, 6, 7, 8]),
    ok.

-spec product_test() -> ok.
product_test() -> 
    1 = product([]),
    2 = product([2]),
    40320 = product([1, 2, 3, 4, 5, 6, 7, 8]),
    ok.

-spec zip_v1_test() -> ok.
zip_v1_test() -> 
    [] = zip_v1([], []),
    [] = zip_v1([], [1, 2, 3]),
    [] = zip_v1([1, 3, 5, 7], []),
    [{1, 2}, {3, 4}] = zip_v1([1, 3, 5, 7], [2, 4]),
    [{1, 2}, {3, 4}, {5, 6}, {7, 8}] = zip_v1([1, 3, 5, 7], [2, 4, 6, 8]),
    ok.

-spec zip_v2_test() -> ok.
zip_v2_test() -> 
    [] = zip_v2([], []),
    [] = zip_v2([], [1, 2, 3]),
    [] = zip_v2([1, 3, 5, 7], []),
    [{1, 2}, {3, 4}] = zip_v2([1, 3, 5, 7], [2, 4]),
    [{1, 2}, {3, 4}, {5, 6}, {7, 8}] = zip_v2([1, 3, 5, 7], [2, 4, 6, 8]),
    ok.


-spec zip_with_v1_test() -> ok.
zip_with_v1_test() ->
    [] = zip_with_v1(fun(X, Y) -> X + Y end, [], []),
    [] = zip_with_v1(fun(X, Y) -> X + Y end, [], [1, 2, 3]),
    [] = zip_with_v1(fun(X, Y) -> X + Y end, [1, 3, 5, 7], []),
    [3, 7] = zip_with_v1(fun(X, Y) -> X + Y end, [1, 3, 5, 7], [2, 4]),
    ok.

-spec zip_with_v2_test() -> ok.
zip_with_v2_test() -> 
    [] = zip_with_v2(fun(X, Y) -> X + Y end, [], []),
    [] = zip_with_v2(fun(X, Y) -> X + Y end, [], [1, 2, 3]),
    [] = zip_with_v2(fun(X, Y) -> X + Y end, [1, 3, 5, 7], []),
    [3, 7] = zip_with_v2(fun(X, Y) -> X + Y end, [1, 3, 5, 7], [2, 4]),
    ok.