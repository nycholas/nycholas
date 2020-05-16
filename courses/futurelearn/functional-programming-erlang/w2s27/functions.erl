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
-module(functions).

-export([join/2, concat/1, member/2, tests/0]).

-spec join([any()], [any()]) -> [any()].
join([], []) ->
    [];
join([], [E | L2]) ->
    [E | join([], L2)];
join([E | L1], L2) ->
    [E | join(L1, L2)].

-spec concat([any()]) -> [any()].
concat([]) ->
    [];
concat([E1 | L]) ->
    join(E1, concat(L)).

-spec member(any(), [any()]) -> 'true' | 'false'.
member(_, []) ->
    false;
member(E1, [E1 | _]) ->
    true;
member(E1, [_ | L]) ->
    member(E1, L).

-spec tests() -> ok.
tests() ->
    %% join
    [] = join([], []),
    [4, 5, 6] = join([], [4, 5, 6]),
    [1, 2, 3] = join([1, 2, 3], []),
    [1, 2, 3, 4, 5, 6] = join([1, 2, 3], [4, 5, 6]),
    "hello" = join("hel", "lo"),

    %% concat
    [] = concat([]),
    "goodbye" = concat(["goo","d","","by","e"]),

    %% member
    true = member(2, [2, 0, 0, 1]),
    false = member(20, [2, 0, 0, 1]),

    %% perms
    % [[]] = perms([]),
    % [[1, 2, 3], [2, 3, 1], [3, 1, 2], [2, 1, 3], [1, 3, 2], [3, 2, 1]] = perms([1, 2, 3]),

    ok.

