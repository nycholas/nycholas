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
-module(mycalc).

-revision('Revision: 0.1 ').
-created('Date: 2011/12/25 15:01:17 ').
-created_by('nycholas@gmail.com').
-modified('Date: 2011/12/25 15:01:17 ').
-modified_by('nycholas@gmail.com').

-export([start/0, stop/1, sum/3, subtract/3, multiplies/3, divides/3, return_operation/2]).
-export([init/1, handle_call/3, handle_cast/2]).

-record(operation, {name, a, b, result}).

%%% Client API
start() ->
    mycalc_server:start_link(?MODULE, []).

stop(Pid) ->
    mycalc_server:call(Pid, terminate).

sum(Pid, A, B) ->
    mycalc_server:call(Pid, {sum, A, B}).

subtract(Pid, A, B) ->
    mycalc_server:call(Pid, {subtract, A, B}).

multiplies(Pid, A, B) ->
    mycalc_server:call(Pid, {multiplies, A, B}).

divides(Pid, A, B) ->
    mycalc_server:call(Pid, {divides, A, B}).

return_operation(Pid, Operation = #operation{}) ->
    mycalc_server:cast(Pid, {return, Operation}).

%%% Server functions
init([]) ->
    []. %% no treatment of info here!

handle_call({sum, A, B}, From, Operations) ->
    mycalc_server:reply(From, [mycalc_sum(A, B)|Operations]);

handle_call({subtract, A, B}, From, Operations) ->
    mycalc_server:reply(From, [mycalc_subtract(A, B)|Operations]);

handle_call({multiplies, A, B}, From, Operations) ->
    mycalc_server:reply(From, [mycalc_multiplies(A, B)|Operations]);

handle_call({divides, A, B}, From, Operations) ->
    mycalc_server:reply(From, [mycalc_divides(A, B)|Operations]);

handle_call(terminate, From, Operations) ->
    mycalc_server:reply(From, ok),
    mycalc_terminate(Operations).

handle_cast({return, Operation = #operation{}}, Operations) ->
    [Operation|Operations].

%%% Private functions
mycalc_make_operation(Name, A, B, Result) ->
    #operation{name=Name, a=A, b=B, result=Result}.

mycalc_sum(A, B) ->
    mycalc_make_operation(sum, A, B, A + B).

mycalc_subtract(A, B) ->
    mycalc_make_operation(subtract, A, B, A - B).

mycalc_multiplies(A, B) ->
    mycalc_make_operation(multiplies, A, B, A * B).

mycalc_divides(A, B) ->
    mycalc_make_operation(divides, A, B, A / B).

mycalc_terminate(Operations) ->
    [io:format("~p(~p, ~p) = ~p, was set free.~n",
        [O#operation.name, O#operation.a,
         O#operation.b, O#operation.result]) || O <- Operations],
    exit(normal).

