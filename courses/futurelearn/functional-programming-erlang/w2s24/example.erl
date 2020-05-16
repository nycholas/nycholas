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

-export([palindrome/1, tests/0]).

-spec palindrome([char()]) -> true | false.
palindrome([]) -> 
    true;
palindrome(S) -> 
    S1 = string_strip(string_lowercase(S)),
    S2 = string_reverse(S1),
    S1 =:= S2.

-spec tests() -> ok.
tests() ->
    %% string_lowercase
    [] = string_lowercase([]),
    "qwert" = string_lowercase("QWERT"),
    "áÇ~abc$0-" = string_lowercase("áÇ~abc$0-"),

    %% string_strip
    [] = string_strip([]),
    "abc" = string_strip("áÇ~abc$0-"),

    %% string_split
    {[], []} = string_split(0, []),
    {"asd", "fghi"} = string_split(3, "asdfghi"),

    %% palindrome
    true = palindrome([]),
    true = palindrome("Madam I\'m Adam"),
    false = palindrome("abcabc"),
    true = palindrome("Roma me tem amor."),
    true = palindrome("Socorram-me, subi no onibus em Marrocos!"),
    true = palindrome("A mala nada na lama."),
    true = palindrome("A grama é amarga."),
    true = palindrome("A Rita, sobre vovo, verbos atira."),
    true = palindrome("Anotaram a data da maratona."),

    ok.

-spec string_lowercase([T]) -> [T].
string_lowercase([]) ->
    [];
string_lowercase([E | L]) when E >= $A, E =< $Z ->
    C = E + ($a - $A),
    [C | string_lowercase(L)];
string_lowercase([E | L]) ->
    [E | string_lowercase(L)].

-spec string_strip([T]) -> [T].
string_strip([]) ->
    [];
string_strip([E | L]) when E >= $A, E =< $Z;
                           E >= $a, E =< $z ->
    [E | string_strip(L)];
string_strip([_E | L]) ->
    string_strip(L).

-spec string_split(integer(), [T]) -> {[T], [T]}.
string_split(N, L) ->
    string_split(N, [], L).

-spec string_split(integer(), [T], [T]) -> {[T], [T]}.
string_split(0, L1, L2) ->
    {string_reverse(L1), L2};
string_split(_, L1, []) ->
    {string_reverse(L1), []};
string_split(N, L1, [E | L2]) ->
    string_split(N - 1, [E | L1], L2).

-spec string_reverse([T]) -> [T].
string_reverse(L) ->
    string_reverse(L, []).

-spec string_reverse([T], [T]) -> [T].
string_reverse([], Acc) ->
    Acc;
string_reverse([E | L], Acc) ->
    string_reverse(L, [E | Acc]).