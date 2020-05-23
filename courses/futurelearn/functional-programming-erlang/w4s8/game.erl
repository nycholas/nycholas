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
-module(game).

-export([beat/1, lose/1, result/2, tournament/2]).

-include_lib("eunit/include/eunit.hrl").

-type gesture() :: rock | paper | scissors.
-type result() :: win | lose | draw.

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