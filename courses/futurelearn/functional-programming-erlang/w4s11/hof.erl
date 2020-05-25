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
-module(hof).

-export([compose/1, twice/1, iterate/1]).

-include_lib("eunit/include/eunit.hrl").

-spec compose([function()]) -> function().
compose(Funs) ->
    lists:foldl(fun compose/2, fun identify/1, Funs).

-spec twice(function()) -> function().
twice(Fun) ->
    compose(Fun, Fun).

iterate(0) ->
    fun identify/1;
iterate(N) ->
    fun(Fun) -> 
        compose([Fun || _ <- lists:seq(0, N)])
    end.

%%====================================================================
%% Internals
%%====================================================================

-spec compose(function(), function()) -> function().
compose(Fun1, Fun2) ->
    fun(X) -> 
        Fun2(Fun1(X))
    end.

-spec add(integer()) -> function().
add(X) -> 
    fun(Y) -> 
        X + Y 
    end.

-spec times(integer()) -> function().
times(X) -> 
    fun(Y) -> 
        X * Y 
    end.

-spec identify(any()) -> any().
identify(X) ->
    X.

%%====================================================================
%% Unit Tests
%%====================================================================

-spec compose_test() -> ok.
compose_test() ->
    %% commutative
    F1 = compose([add(N) || N <- lists:seq(0, 10)]),
    F2 = compose([add(N) || N <- lists:seq(10, 0, -1)]),
    true = F1(1) == F2(1),

    %% commutative
    F3 = compose([times(N) || N <- lists:seq(1, 10)]),
    F4 = compose([times(N) || N <- lists:seq(10, 1, -1)]),
    true = F3(1) == F4(1),

    %% noncommutative
    G1 = fun(X) -> (2 * X) + 1 end,
    G2 = fun(X) -> (3 * X) + 7 end,
    F5 = compose([G1, G2]),
    F6 = compose([G2, G1]),
    true = F5(1) /= F6(1),

    %% noncommutative
    H1 = compose([add(1), times(2)]),
    H2 = compose([times(2), add(1)]),
    true = H1(1) /= H2(1),
    ok.

-spec twice_test() -> ok.
twice_test() ->
    F1 = twice(times(3)), 
    18 = F1(2), %% 18 = (2 * 3) * 3.

    F2 = twice(twice(times(3))),
    162 = F2(2), %% 18 = (((2 * 3) * 3) * 3) * 3.
    ok.

-spec iterate_test() -> ok.
iterate_test() ->
    F1 = iterate(0),
    G1 = F1(fun (X) -> X + 1 end),
    124 = G1(123),

    F2 = iterate(10),
    G2 = F2(add(1)),
    12 = G2(1),

    F3 = iterate(10),
    G3 = F3(times(2)),
    2048 = G3(1),
    ok.