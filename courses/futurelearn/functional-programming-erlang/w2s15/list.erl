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