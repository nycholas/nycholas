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
-module(index).

-export([file/1, get_file_contents/1, show_file_contents/1, tests/0]).

% Used to read a file into a list of lines.
% Example files available in:
%   gettysburg-address.txt (short)
%   dickens-christmas.txt  (long)

-spec file(nonempty_string()) -> list(tuple()).
file(FileName) ->
    Lines = get_file_contents(FileName),
    Normalized = normalize_all_lines(Lines),
    Tokenized = tokenize_all_lines(Normalized),
    TokenizedNorm = remove_short_token_all_lines(Tokenized),
    TokenOccurs = token_occurs_all_lines(TokenizedNorm),
    SummaryOccurs = summarize_token_occurs(TokenOccurs),
    sort_occurs(SummaryOccurs).

% Get the contents of a text file into a list of lines.
% Each line has its trailing newline removed.

-spec get_file_contents(nonempty_string()) -> list().
get_file_contents(Name) ->
    {ok, File} = file:open(Name, [read]),
    Rev = get_all_lines(File, []),
    lists:reverse(Rev).

% Auxiliary function for get_file_contents.
% Not exported.

-spec get_all_lines(pid(), list(string())) -> list(string()).
get_all_lines(File, Partial) ->
    case io:get_line(File, "") of
      eof -> file:close(File), Partial;
      Line ->
	  {Strip, _} = lists:split(length(Line) - 1, Line),
	  get_all_lines(File, [Strip | Partial])
    end.

-spec normalize_all_lines(list()) -> list().
normalize_all_lines(Lines) ->
    LowercaseLines = lists_map(fun(E) -> string_lowercase(E) end, Lines),
    lists_map(fun(E) -> string_strip(E) end, LowercaseLines).

-spec tokenize_all_lines(list()) -> list().
tokenize_all_lines(Lines) -> 
    lists_map(fun(E) -> string_tokenize(E) end, Lines).

-spec remove_short_token_all_lines(list()) -> list().
remove_short_token_all_lines([]) ->
    [];
remove_short_token_all_lines([Line | Lines]) ->
    [lists_filter(fun(E) -> string_length(E) > 3 end, Line) | remove_short_token_all_lines(Lines)].

-spec token_occurs_all_lines(list()) -> list(tuple()).
token_occurs_all_lines(Lines) ->
    token_occurs_all_lines(Lines, 1, []).

-spec token_occurs_all_lines(list(), integer(), list(tuple())) -> list(tuple()).
token_occurs_all_lines([], _Index, ListKey) ->
    ListKey;
token_occurs_all_lines([Line | Lines], Index, ListKey) ->
    token_occurs_all_lines(Lines, Index + 1, token_occurs_by_line(Line, Index, ListKey)).

-spec token_occurs_by_line(list(tuple()), integer(), list(tuple())) -> list(tuple()).
token_occurs_by_line([], _Index, ListKey) ->
    ListKey;
token_occurs_by_line([Token | Line], Index, ListKey) ->
    NewListKey = case lists_keysearch(Token, ListKey) of
        {Token, ListOccurs} ->
            [{Token, [Index | ListOccurs]} | lists_keydelete(Token, ListKey)];
        _ ->
            [{Token, [Index]} | ListKey]
    end,
    token_occurs_by_line(Line, Index, NewListKey).

-spec summarize_token_occurs(list(tuple())) -> list(tuple()).
summarize_token_occurs([]) ->
    [];
summarize_token_occurs([{Token, Occurs} | ListKey]) ->
    [{Token, summarize_occurs(lists_reverse(Occurs))} | summarize_token_occurs(ListKey)].

-spec summarize_occurs(list(tuple())) -> list(tuple()).
summarize_occurs([N]) ->
    [{N, N}];
summarize_occurs([N | Occurs]) ->
    summarize_occurs(Occurs, N, 1, []).

-spec summarize_occurs(list(), integer(), integer(), list()) -> list(tuple()).
summarize_occurs([], _Curr, _Index, Acc) ->
    Acc;
summarize_occurs([N | Occurs], -1, _Index, Acc) -> %% XXX: -1 for next number
    summarize_occurs(Occurs, N, 1, Acc);
summarize_occurs([N | Occurs], N, Index, Acc) ->
    summarize_occurs(Occurs, N, Index, Acc);
summarize_occurs([M | Occurs], N, Index, Acc) when M + Index == N ->
    summarize_occurs(Occurs, N, Index + 1, Acc);
summarize_occurs([M | Occurs], N, Index, Acc) when M + Index > N ->
    summarize_occurs(Occurs, -1, Index, [{N, M} | Acc]).

-spec sort_occurs(list(tuple())) -> list(tuple()).
sort_occurs(Occurs) ->
    lists_keysort(Occurs).

% Show the contents of a list of strings.
% Can be used to check the results of calling get_file_contents.

-spec show_file_contents(list()) -> ok.
show_file_contents([]) -> ok;
show_file_contents([L | Ls]) ->
    io:format("> \"~s\"~n", [L]),
    show_file_contents(Ls).

-spec tests() -> ok.
tests() ->
    %% string_lowercase
    [] = string_lowercase([]),
    "qwert" = string_lowercase("QWERT"),
    "áÇ~abc$0-" = string_lowercase("áÇ~abc$0-"),

    %% string_strip
    [] = string_strip([]),
    "abc" = string_strip("áÇ~abc$0-"),

    %% string_reverse
    [] = string_reverse([]),
    "cba" = string_reverse("abc"),

    %% string_tokenize
    [[]] = string_tokenize([]),
    ["abc", "def", "xyz"] = string_tokenize("abc def xyz"),

    %% string_length
    0 = string_length([]),
    3 = string_length([1, 4, 5]),

    %% lists_map
    [] = lists_map(fun(N) -> N * 2 end, []),
    [2, 4, 6, 8] = lists_map(fun(N) -> N * 2 end, [1, 2, 3, 4]),

    %% lists_filter
    [] = lists_filter(fun(N) -> N rem 2 == 0 end, []),
    [2, 4, 6] = lists_filter(fun(N) -> N rem 2 == 0 end, [1, 2, 3, 4, 5, 6]),

    %% lists_merge
    [] = lists_merge([], []),
    [1, 2, 3, 4, 5, 6] = lists_merge([1, 2, 3], [4, 5, 6]),
    [1, 2, 3, -2, 4, 5, 6, -1] = lists_merge([1, 2, 3, -2], [4, 5, 6, -1]),

    %% lists_reverse
    [] = lists_reverse([]),
    [3, 2, 1] = lists_reverse([1, 2, 3]),

    %% lists_keymember
    false = lists_keymember(key, []),
    true = lists_keymember(k2, [{k1, "v1"}, {k2, "v2"}, {k3, "v3"}]),

    %% lists_keysearch
    false = lists_keysearch(key, []),
    false = lists_keysearch(k4, [{k1, "v1"}, {k2, "v2"}, {k3, "v3"}]),
    {k2, "v2"} = lists_keysearch(k2, [{k1, "v1"}, {k2, "v2"}, {k3, "v3"}]),

    %% lists_keydelete
    [] = lists_keydelete(key, []),
    [{k1, "v1"}, {k2, "v2"}, {k3, "v3"}] = lists_keydelete(k4, [{k1, "v1"}, {k2, "v2"}, {k3, "v3"}]),
    [{k1, "v1"}, {k3, "v3"}] = lists_keydelete(k2, [{k1, "v1"}, {k2, "v2"}, {k3, "v3"}]),

    %% lists_keysort
    [] = lists_keysort([]),
    [{"a", [2]}, {"b", [1]}, {"c", [3]}] = lists_keysort([{"b", [1]}, {"a", [2]}, {"c", [3]}]),

    Lines = [
        "aaaa bbb ccccc dddd",
        "aaa bbb aaaa dddd eeee",
        "dddd ccccc llll"
    ],
    Normalized = normalize_all_lines(Lines),
    Tokenized = tokenize_all_lines(Normalized),
    TokenizedNorm = remove_short_token_all_lines(Tokenized),
    TokenOccurs = token_occurs_all_lines(TokenizedNorm),
    SummaryOccurs = summarize_token_occurs(TokenOccurs),
    [
        {"aaaa", [{1, 2}]},
        {"ccccc", [{1, 3}]},
        {"dddd", [{1, 2}]},
        {"eeee", [{2, 2}]},
        {"llll", [{3, 3}]}
    ] = sort_occurs(SummaryOccurs),
    
    ok.

-spec lists_map(function(), [T]) -> [T].
lists_map(_Fn, []) ->
    [];
lists_map(Fn, [E | L]) ->
    [Fn(E) | lists_map(Fn, L)].

-spec lists_filter(function(), [T]) -> [T].
lists_filter(_Fn, []) ->
    [];
lists_filter(Fn, [E | L]) ->
    case Fn(E) of
        true ->
            [E | lists_filter(Fn, L)];
        _ ->
            lists_filter(Fn, L)
    end.

-spec lists_merge([T1], [T2]) -> [T1 | T2].
lists_merge(L1, []) ->
    L1;
lists_merge(L1, L2) ->
    L3 = lists_reverse(L1),
    lists_merge(L3, L2, L2).

-spec lists_merge([T1], [T2], [T1 | T2]) -> [T1 | T2].
lists_merge([], _, Acc) ->
    Acc;
lists_merge([E | L1], L2, Acc) ->
    lists_merge(L1, L2, [E | Acc]).

-spec lists_reverse([T]) -> [T].
lists_reverse(L) ->
    lists_reverse(L, []).

-spec lists_reverse([T], [T]) -> [T].
lists_reverse([], Acc) ->
    Acc;
lists_reverse([E | L], Acc) ->
    lists_reverse(L, [E | Acc]).

-spec lists_keymember(any(), [{_, _}]) -> boolean().
lists_keymember(_, []) ->
    false;
lists_keymember(K1, [{K1, _} | _]) ->
    true;
lists_keymember(K1, [_ | L]) ->
    lists_keymember(K1, L).

-spec lists_keysearch(any(), [{_, _}]) -> {_, _} | false.
lists_keysearch(_, []) ->
    false;
lists_keysearch(K1, [{K1, V} | _]) ->
    {K1, V};
lists_keysearch(K1, [_ | L]) ->
    lists_keysearch(K1, L).

-spec lists_keydelete(_, [{_, _}]) -> [{_, _}].
lists_keydelete(_, []) ->
    [];
lists_keydelete(K1, [{K1, _} | L]) ->
    L;
lists_keydelete(K1, [K2 | L]) ->
    [K2 | lists_keydelete(K1, L)].

-spec lists_keysort([tuple()]) -> [tuple()].
lists_keysort([]) ->
    [];
lists_keysort([E | L]) ->
    {Smaller, Larger} = lists_keypartition(E, L, [], []),
    L1 = lists_merge(lists_keysort(Smaller), [E]),
    lists_merge(L1, lists_keysort(Larger)).

-spec lists_keypartition({_, _}, [{_, _}], [{_, _}], [{_, _}]) -> {[{_, _}], [{_, _}]}.
lists_keypartition(_, [], Smaller, Larger) -> 
    {Smaller, Larger};
lists_keypartition({K1, _} = E1, [{K2, _} = E2 | L], Smaller, Larger) -> 
    case K1 >= K2 of
        true ->
            lists_keypartition(E1, L, [E2 | Smaller], Larger);
        _ ->
            lists_keypartition(E1, L, Smaller, [E2 | Larger])
    end.

-spec string_length(string()) -> integer().
string_length([]) ->
    0;
string_length([_E | L]) ->
    1 + string_length(L).

-spec string_lowercase(string()) -> string().
string_lowercase([]) ->
    [];
string_lowercase([E | L]) when E >= $A, E =< $Z ->
    C = E + ($a - $A),
    [C | string_lowercase(L)];
string_lowercase([E | L]) ->
    [E | string_lowercase(L)].

-spec string_strip(string()) -> string().
string_strip([]) ->
    [];
string_strip([E | L]) when E =:= $\s;
                           E >= $A, E =< $Z;
                           E >= $a, E =< $z ->
    [E | string_strip(L)];
string_strip([_E | L]) ->
    string_strip(L).

-spec string_tokenize(string()) -> [string()].
string_tokenize(L) ->
    string_tokenize(L, []).

-spec string_tokenize(string(), string()) -> [string()].
string_tokenize([], Acc) ->
    [string_reverse(Acc)];
string_tokenize([E | L], Acc) when E =:= $\s ->
    [string_reverse(Acc) | string_tokenize(L, [])];
string_tokenize([E | L], Acc) ->
    string_tokenize(L, [E | Acc]).

-spec string_reverse(string()) -> string().
string_reverse(L) ->
    string_reverse(L, []).

-spec string_reverse(string(), string()) -> string().
string_reverse([], Acc) ->
    Acc;
string_reverse([E | L], Acc) ->
    string_reverse(L, [E | Acc]).