-module(example).

-export([palindrome/1, tests/0]).

-spec palindrome(list) -> true | false.
palindrome([]) -> 
    true;
palindrome(S) -> 
    S1 = string_strip(string_lowercase(S)),
    S2 = lists_reverse(S1),
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
    {lists_reverse(L1), L2};
string_split(_, L1, []) ->
    {lists_reverse(L1), []};
string_split(N, L1, [E | L2]) ->
    string_split(N - 1, [E | L1], L2).

-spec lists_reverse([T]) -> [T].
lists_reverse(L) ->
    lists_reverse(L, []).

-spec lists_reverse([T], [T]) -> [T].
lists_reverse([], Acc) ->
    Acc;
lists_reverse([E | L], Acc) ->
    lists_reverse(L, [E | Acc]).