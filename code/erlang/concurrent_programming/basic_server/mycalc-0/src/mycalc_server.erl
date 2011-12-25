%%%----------------------------------------------------------------------------
%%% Basic server calculator.
%%% Copyright (c) 2011, Nycholas de Oliveira e Oliveira <nycholas@gmail.com>
%%% All rights reserved.
%%%
%%% Redistribution and use in source and binary forms, with or without
%%% modification, are permitted provided that the following conditions are met:
%%%
%%% % Redistributions of source code must retain the above copyright notice,
%%%    this list of conditions and the following disclaimer.
%%% % Redistributions in binary form must reproduce the above copyright notice,
%%%    this list of conditions and the following disclaimer in the documentation
%%%    and/or other materials provided with the distribution.
%%% % Neither the name of the Nycholas de Oliveira e Oliveira nor the names of
%%%    its contributors may be used to endorse or promote products derived from
%%%    this software without specific prior written permission.
%%%
%%% THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS"
%%% AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
%%% IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
%%% ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE
%%% LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR
%%% CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF
%%% SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS
%%% INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN
%%% CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE)
%%% ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
%%% POSSIBILITY OF SUCH DAMAGE.
%%%----------------------------------------------------------------------------
-module(mycalc_server).

-revision('Revision: 0.1 ').
-created('Date: 2011/12/25 15:01:17 ').
-created_by('nycholas@gmail.com').
-modified('Date: 2011/12/25 15:01:17 ').
-modified_by('nycholas@gmail.com').

-export([start/0, stop/1, sum/3, subtract/3, multiplies/3, divides/3]).

-record(operation, {name, a, b, result}).

%%% Client API
start() ->
    spawn_link(fun init/0).

stop(Pid) ->
    rpc(Pid, terminate).

sum(Pid, A, B) ->
    rpc(Pid, {sum, A, B}).

subtract(Pid, A, B) ->
    rpc(Pid, {subtract, A, B}).

multiplies(Pid, A, B) ->
    rpc(Pid, {multiplies, A, B}).

divides(Pid, A, B) ->
    rpc(Pid, {divides, A, B}).

%%% Server functions
init() ->
    loop([]).

rpc(Pid, Message) ->
    Ref = erlang:monitor(process, Pid),
    Pid ! {self(), Ref, Message},
    receive
        {Ref, Reply} ->
            erlang:demonitor(Ref, [flush]),
            Reply;
        {'DOWN', Ref, process, Pid,  Reason} ->
            erlang:error(Reason)
    after 5000 ->
        erlang:error(timeout)
    end.
    
loop(Operations) ->
    receive
        {From, Ref, {sum, A, B}} ->
            N = mycalc_sum(A, B),
            Operation = mycalc_make_operation(sum, A, B, N),
            From ! {Ref, N},
            loop([Operation|Operations]);
        {From, Ref, {subtract, A, B}} ->
            N = mycalc_subtract(A, B),
            Operation = mycalc_make_operation(subtract, A, B, N),
            From ! {Ref, N},
            loop([Operation|Operations]);
        {From, Ref, {multiplies, A, B}} ->
            N = mycalc_multiplies(A, B),
            Operation = mycalc_make_operation(multiplies, A, B, N),
            From ! {Ref, N},
            loop([Operation|Operations]);
        {From, Ref, {divides, A, B}} ->
            N = mycalc_divides(A, B),
            Operation = mycalc_make_operation(divides, A, B, N),
            From ! {Ref, N},
            loop([Operation|Operations]);
        {From, Ref, terminate} ->
            From ! {Ref, ok},
            mycalc_terminate(Operations);
        Unknown ->
            io:format("Unknown message: ~p~n", [Unknown]),
            loop(Operations)
    end.

%%% Private functions
mycalc_make_operation(Name, A, B, Result) ->
    #operation{name=Name, a=A, b=B, result=Result}.

mycalc_sum(A, B) ->
    A + B.

mycalc_subtract(A, B) ->
    A - B.

mycalc_multiplies(A, B) ->
    A * B.

mycalc_divides(A, B) ->
    A / B.

mycalc_terminate(Operations) ->
    [io:format("~p(~p, ~p) = ~p, was set free.~n",
        [O#operation.name, O#operation.a,
         O#operation.b, O#operation.result]) || O <- Operations],
    ok.