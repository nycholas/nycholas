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
-module(exercises).

-export([area/1, perimeter/1, enclose/1, bits/1, tests/0]).

area({circle, {_X, _Y}, R}) ->
    math:pi() * R * R;
area({rectangle, {_X, _Y}, H, W}) ->
    H * W;
area({triangle, X = {_X1, _Y1}, Y = {_X2, _Y2}, Z = {_X3, _Y3}}) ->
    A = distance(X, Y),
    B = distance(Y, Z),
    C = distance(Z, X),
    S =  (A + B + C) / 2,
    math:sqrt(S * (S - A) * (S - B) * (S - C)).
    
perimeter({circle, {_X, _Y}, R}) ->
    2 * math:pi() * R;
perimeter({rectangle, {_X, _Y}, H, W}) ->
    2 * (H + W);
perimeter({triangle, X = {_X1, _Y1}, Y = {_X2, _Y2}, Z = {_X3, _Y3}}) ->
    A = distance(X, Y),
    B = distance(Y, Z),
    C = distance(Z, X),
    A + B + C.

enclose({circle, {X, Y}, R}) ->
    {rectangle, {X - R, Y - R}, 2 * R, 2 * R};
enclose({rectangle, {X, Y}, H, W}) ->
    {rectangle, {X, Y}, H, W};
enclose({triangle, {X1, Y1}, {X2, Y2}, {X3, Y3}}) ->
    XMin = lists:min([X1, X2, X3]),
    YMin = lists:min([Y1, Y2, Y3]),
    XMax = lists:max([X1, X2, X3]),
    YMax = lists:max([Y1, Y2, Y3]),
    {rectangle, {XMin, YMin}, XMax - XMin, YMax - YMin}.

bits(N) when N > 0  -> 
    lists:foldr(fun(M, Acc) -> M + Acc end, 0, [X || <<X:1>> <= <<N:64>>]).

tests() -> 
    %% Circle
    28.28 = truncate(area({circle, {0, 0}, 3})),
    213.63 = truncate(perimeter({circle, {0, 0}, 34})),
    {rectangle, {-3, -3}, 6, 6} = enclose({circle, {0, 0}, 3}),

    %% Rectangle
    6 = area({rectangle, {1, 1}, 3, 2}),
    10 = perimeter({rectangle, {1, 1}, 3, 2}),
    {rectangle, {1, 1}, 3, 2} = enclose({rectangle, {1, 1}, 3, 2}),

    %% Triangle
    16.5 = area({triangle, {-2, 2}, {1, 5}, {6, -1}}),
    23.56 = truncate(perimeter({triangle, {1, 2}, {3, -4}, {-4, 5}})),
    {rectangle, {1, 1}, 3, 2} = enclose({triangle, {2, 1}, {4, 2}, {1, 3}}),

    %% Bits
    3 = bits(7), %% 0b111
    1 = bits(8), %% 0b1000
    2 = bits(10), %% 0b1010
    ok.

distance({X1, Y1}, {X2, Y2}) ->
    math:sqrt(math:pow(X2 - X1, 2) + math:pow(Y2 - Y1, 2)).

truncate(N) when is_number(N) ->
    truncate(N, 2).

truncate(N, Decimals) when is_number(N); 
                           is_integer(Decimals) ->
    M = lists:foldr(fun(E, Acc) -> E * Acc end, 1, [10 || _ <- lists:seq(1, Decimals)]),
    math:ceil(N * M) / M.