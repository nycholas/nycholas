-module(palin).

-export([palindrome/1]).

-spec palindrome(string()) -> boolean().
palindrome(Xs) ->
    palin(nocaps(nopunct(Xs))).

-spec nopunct(string()) -> string().
nopunct([]) ->
    [];
nopunct([X | Xs]) ->
    case lists:member(X, "., ;:\t\n'\"") of
      true ->
          nopunct(Xs);
      false ->
          [X | nopunct(Xs)]
    end.

-spec nocaps(string()) -> string().
nocaps([]) ->
    [];
nocaps([X | Xs]) ->
    [nocap(X) | nocaps(Xs)].

-spec nocap(char()) -> char().
nocap(X) ->
    case $A =< X andalso X =< $Z of
      true ->
          X + 32;
      false ->
          X
    end.

-spec palin(string()) -> boolean().
palin(Xs) ->
    Xs == reverse(Xs).

-spec reverse(list()) -> list().
reverse(Xs) ->
    shunt(Xs, []).

-spec shunt(list(), list()) -> list().
shunt([], Ys) ->
    Ys;
shunt([X | Xs], Ys) ->
    shunt(Xs, [X | Ys]).

