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
    
