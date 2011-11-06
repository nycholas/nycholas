%%%----------------------------------------------------------------------------
%%% Example OTP with TCP server.
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
-module(socket_server).

-revision('Revision: 0.1 ').
-created('Date: 2011/02/18 11:02:00 ').
-created_by('nycholas@gmail.com').
-modified('Date: 2011/02/18 11:02:00 ').
-modified_by('nycholas@gmail.com').

-behaviour(gen_server).

-export([start/3]).
-export([accept_loop/1]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-define(TCP_OPTIONS, [binary, {packet, 0}, {active, false}, {reuseaddr, true}]).

-record(server_state, {port, loop, ip=any, lsocket=null}).

start(Name, Port, Loop) ->
    State = #server_state{port=Port, loop=Loop},
    gen_server:start_link({local, Name}, ?MODULE, State, []).
    
accept_loop({Server, LSocket, {Module, Function}}) ->
    {ok, Socket} = gen_tcp:accept(LSocket),
    gen_server:cast(Server, {accepted, self()}),
    Module:Function(Socket).
    
accept(State = #server_state{lsocket=LSocket, loop=Loop}) ->
    proc_lib:spawn(?MODULE, accept_loop, [{self(), LSocket, Loop}]),
    State.

init(#server_state{port=Port} = State) ->
    case gen_tcp:listen(Port, ?TCP_OPTIONS) of
        {ok, LSocket} ->
            NewState = State#server_state{lsocket=LSocket},
            {ok, accept(NewState)};
        {error, Reason} ->
            {stop, Reason}
    end.

handle_call({sum, A, B}, _From, State) ->
    Reply = mycalc:sum(A, B),
    {reply, Reply, State};
handle_call({subtract, A, B}, _From, State) ->
    Reply = mycalc:subtract(A, B),
    {reply, Reply, State};
handle_call({multiplies, A, B}, _From, State) ->
    Reply = mycalc:multiplies(A, B),
    {reply, Reply, State};
handle_call({divides, A, B}, _From, State) ->
    Reply = mycalc:divides(A, B),
    {reply, Reply, State};
handle_call(stop, _From, State) ->
    {stop, normal, stopped, State};
handle_call(_Msg, _From, State) ->
    {noreply, State}.

handle_cast({accepted, _Pid}, State=#server_state{}) ->
    {noreply, accept(State)};
handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
