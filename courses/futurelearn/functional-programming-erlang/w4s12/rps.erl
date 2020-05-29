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
-module(rps).

-export([play/1, play_two/3, 
         result/2, tournament/2, 
         echo/1, rock/1, no_repeat/1, const/1, cycle/1, rand/1, least_frequent/1, most_frequent/1,
         rand_strategy/1, best_strategy/1]).

-include_lib("eunit/include/eunit.hrl").

-type gesture() :: rock | paper | scissors.
-type short_gesture() :: r | p | s.
-type strategy() :: function().
-type result() :: win | lose | draw.
-type overall_result() :: {non_neg_integer(), non_neg_integer()}.

%
% play one strategy against another, for N moves.
%

-spec play_two(strategy(), strategy(), pos_integer()) -> overall_result().
play_two(StrategyL, StrategyR, N) ->
    play_two(StrategyL, StrategyR, [], [], N).

% tail recursive loop for play_two/3
% 0 case computes the result of the tournament

-spec play_two(strategy(), strategy(), [gesture()], [gesture()], non_neg_integer()) -> overall_result().
play_two(_, _, PlaysL, PlaysR, 0) ->
    play_two_result(PlaysL, PlaysR, 0, 0);
play_two(StrategyL, StrategyR, PlaysL, PlaysR, N) ->
   play_two(StrategyL, StrategyR, [StrategyL(PlaysL) | PlaysL], [StrategyR(PlaysR) | PlaysR], N - 1).

%
% interactively play against a strategy, provided as argument.
%

-spec play(strategy()) -> overall_result().
play(Strategy) ->
    io:format("Rock - paper - scissors~n"),
    io:format("Play one of rock, paper, scissors, ...~n"),
    io:format("... r, p, s, stop, followed by '.'~n"),
    play(Strategy, [], 0, 0).

% tail recursive loop for play/1

-spec play(strategy(), [gesture()], non_neg_integer(), non_neg_integer()) -> overall_result().
play(Strategy, Moves, ResultL, ResultR) ->
    {ok, P1} = io:read("Play 1: "),
    Play1 = expand(P1),
    case Play1 of
	    stop ->
	        io:format("Stopped~n"),
            {ResultL, ResultR};
	    _ ->
            Play2 = Strategy(Moves),
	        io:format("Play 2: ~p~n", [Play2]),
	        Result = result(Play1, Play2),
	        io:format("Result: ~p~n", [Result]),
            case Result of
                win ->
                    play(Strategy, [Play1 | Moves], ResultL + 1, ResultR),
                    {ResultL + 1, ResultR};
                lose ->
                    play(Strategy, [Play1 | Moves], ResultL, ResultR + 1),
                    {ResultL, ResultR + 1};
                draw ->
                    play(Strategy, [Play1 | Moves], ResultL, ResultR),
                    {ResultL, ResultR}
            end
    end.

-spec result(gesture(), gesture()) -> result().
result(Player1, Player1) ->
    draw;
result(Player1, Player2) ->
    case (lose(Player1) == Player2) orelse (Player1 == beat(Player2)) of
        true ->
            win;
        false ->
            lose
    end.

-spec tournament([gesture()], [gesture()]) -> integer().
tournament(Player1, Player2) ->
    Results = lists:map(fun({P1, P2}) -> result(P1, P2)  end, lists:zip(Player1, Player2)),
    lists:foldr(
        fun (win, Acc) -> Acc + 1;
            (lose, Acc) -> Acc - 1; 
            (draw, Acc) -> Acc 
        end, 0, Results).

%
% strategies.
%
-spec echo([gesture()]) -> gesture().
echo([]) ->
    paper;
echo([Last | _]) ->
    Last.

-spec rock([gesture()]) -> rock.
rock(_) ->
    rock.

-spec no_repeat([gesture()]) -> gesture().
no_repeat([]) ->
    paper;
no_repeat([X | _]) ->
    beat(X).

-spec const(gesture()) -> function().
const(Play) ->
    fun(_) -> 
        Play 
    end.

-spec cycle([gesture()]) -> gesture().
cycle(Plays) ->
    enum(length(Plays) rem 3).

-spec rand([gesture()]) -> gesture().
rand(_) ->
    enum(rand:uniform(3) - 1).

-spec least_frequent([gesture()]) -> gesture().
least_frequent([]) ->
    rand([]);
least_frequent(Plays) ->
    [{Play, _} | _] = play_frequency(Plays),
    Play.

-spec most_frequent([gesture()]) -> gesture().
most_frequent([]) ->
    rand([]);
most_frequent(Plays) ->
    {Play, _} = lists:last(play_frequency(Plays)),
    Play.

-spec rand_strategy([strategy()]) -> strategy().
rand_strategy([]) ->
    fun(Plays) ->
        rand(Plays)
    end;
rand_strategy(Strategies) ->
    fun(Plays) ->
        Strategy = lists:nth(rand:uniform(length(Strategies)), Strategies),
        Strategy(Plays)
    end.

-spec best_strategy([strategy()]) -> strategy().
best_strategy([]) ->
    fun(Plays) ->
        rand(Plays)
    end;
best_strategy(Strategies) ->
    fun(Plays) ->
        StrategyScore = lists:map(
            fun(Strategy) -> 
                SPlays = lists:duplicate(length(Plays), Strategy(Plays)),
                Score = tournament(SPlays, Plays),
                {Strategy, Score}
            end, Strategies),
        [{S, _} | _] = lists:keysort(2, StrategyScore),
        S(Plays)
    end.

%%====================================================================
%% Internals
%%====================================================================

-spec enum(integer()) -> gesture().
enum(0) ->
    rock;
enum(1) ->
    paper;
enum(2) ->
    scissors.

-spec expand(short_gesture()) -> gesture() | stop.
expand(r) -> 
    rock;
expand(p) -> 
    paper;		    
expand(s) -> 
    scissors;
expand(_) -> 
    stop.

-spec beat(gesture()) -> gesture().
beat(rock) -> 
    paper;
beat(paper) -> 
    scissors;
beat(scissors) -> 
    rock.

-spec lose(gesture()) -> gesture().
lose(rock) ->
    scissors;
lose(paper) ->
    rock;
lose(scissors) ->
    paper.

-spec play_two_result([gesture()], [gesture()], non_neg_integer(), non_neg_integer()) -> overall_result().
play_two_result([], [], Result, Result) ->
    {Result, Result};
play_two_result([], [], ResultL, ResultR) when ResultL > ResultR ->
    {ResultL, ResultR};
play_two_result([], [], ResultL, ResultR) when ResultL < ResultR ->
    {ResultL, ResultR};
play_two_result([PlayL | PlaysL], [PlayR | PlaysR], ResultL, ResultR) ->
    case {ResultL > ResultR + length(PlaysR), ResultL + length(PlaysL) < ResultR} of 
        {true, false} ->
            play_two_result([], [], ResultL, ResultR);
        {false, true} ->
            play_two_result([], [], ResultL, ResultR);
        _ ->
            case result(PlayL, PlayR) of 
                win -> 
                    play_two_result(PlaysL, PlaysR, ResultL + 1, ResultR);
                lose ->
                    play_two_result(PlaysL, PlaysR, ResultL, ResultR + 1);
                draw -> 
                    play_two_result(PlaysL, PlaysR, ResultL, ResultR)
            end
    end.

-spec play_frequency([gesture()]) -> [{gesture(), non_neg_integer()}].
play_frequency(Plays) ->
    play_frequency(Plays, []).

-spec play_frequency([gesture()], [{gesture(), non_neg_integer()}]) -> [{gesture(), non_neg_integer()}].
play_frequency([], Acc) ->
    lists:keysort(2, Acc);
play_frequency([Play | Plays], Acc) ->
    case lists:keysearch(Play, 1, Acc) of
        {value, {Play, Value}} ->
            play_frequency(Plays, lists:keyreplace(Play, 1, Acc, {Play, Value + 1}));
        false ->
            play_frequency(Plays, [{Play, 1} | Acc])
    end.

%%====================================================================
%% Unit Tests
%%====================================================================

-spec result_test() -> ok.
result_test() ->
    lose = result(rock, paper),
    draw = result(rock, rock),
    win = result(rock, scissors),
    lose = result(paper, scissors),
    win = result(paper, rock),
    ok.

-spec tournament_test() -> ok.
tournament_test() -> 
    0 = tournament([], []),
    0 = tournament([rock], [rock]),
    1 = tournament([paper], [rock]),
    -1 = tournament([paper], [scissors]),
    -1 = tournament([rock, rock, paper, paper], [rock, paper, scissors, rock]),
    1 = tournament([rock, scissors, rock, paper], [rock, paper, paper, rock]),
    0 = tournament([rock, rock, paper, paper], [rock, rock, paper, paper]),
    ok.

-spec least_frequent_test() -> ok.
least_frequent_test() ->
    _ = least_frequent([]),
    scissors = least_frequent([rock, rock, paper, scissors, scissors, paper, rock]),
    scissors = least_frequent([rock, paper, scissors]),
    ok.

-spec most_frequent_test() -> ok.
most_frequent_test() ->
    _ = most_frequent([]),
    rock = most_frequent([rock, rock, paper, scissors, scissors, paper, rock]),
    rock = most_frequent([rock, paper, scissors]),
    ok.

-spec rand_strategy_test() -> ok.
rand_strategy_test() ->
    S1 = rand_strategy([const(rock), fun rock/1]),
    rock = S1([rock, paper, scissors]),
    rock = S1([rock, paper, scissors]),
    rock = S1([rock, paper, scissors]),

    S2 = rand_strategy([fun rand/1, fun least_frequent/1, fun most_frequent/1]),
    _ = S2([rock, paper, scissors]),
    _ = S2([rock, paper, scissors]),
    _ = S2([rock, paper, scissors]),
    ok.

-spec best_strategy_test() -> ok.
best_strategy_test() ->
    S1 = best_strategy([const(rock), fun rock/1]),
    rock = S1([rock, paper, scissors]),
    rock = S1([rock, paper, scissors]),
    rock = S1([rock, paper, scissors]),

    S2 = best_strategy([fun rand/1, fun least_frequent/1, fun most_frequent/1]),
    _ = S2([rock, paper, scissors]),
    _ = S2([rock, paper, scissors]),
    _ = S2([rock, paper, scissors]),
    ok.

-spec play_two_test() -> ok.
play_two_test() ->
    {0, 0} = play_two(fun rock/1, fun rock/1, 5),
    {_, _} = play_two(fun echo/1, fun no_repeat/1, 5),
    {_, _} = play_two(const(rock), fun cycle/1, 5),
    {_, _} = play_two(fun least_frequent/1, fun most_frequent/1, 10),
    {_, _} = play_two(fun rand/1, fun rand/1, 10),
    ok.