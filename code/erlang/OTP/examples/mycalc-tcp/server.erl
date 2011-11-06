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
-module(server).
-export([start/0, loop/1, stop/0]).

start() ->
    socket_server:start(?MODULE, 7000, {?MODULE, loop}).

loop(Socket) ->
    case gen_tcp:recv(Socket, 0) of
        {ok, Bin} ->
            Term = binary_to_term(Bin),
            io:format("Server received binary = ~p~n", [Bin]),
            io:format("Server (unpacked) = ~p~n", [Term]),
            case Term of
                {sum, A, B} ->
                    N = sum(A, B),
                    ok = gen_tcp:send(Socket, term_to_binary(N));
                {subtract, A, B} ->
                    N = subtract(A, B),
                    ok = gen_tcp:send(Socket, term_to_binary(N));
                {multiplies, A, B} ->
                    N = multiplies(A, B),
                    ok = gen_tcp:send(Socket, term_to_binary(N));
                {divides, A, B} ->
                    N = divides(A, B),
                    ok = gen_tcp:send(Socket, term_to_binary(N));
                _ ->
                    ok = gen_tcp:send(Socket, Bin)
            end,
            loop(Socket);
        {error, closed} ->
            ok
    end.

stop() ->
    gen_server:call(?MODULE, stop).

sum(A, B) ->
    gen_server:call(?MODULE, {sum, A, B}).

subtract(A, B) ->
    gen_server:call(?MODULE, {subtract, A, B}).

multiplies(A, B) ->
    gen_server:call(?MODULE, {multiplies, A, B}).

divides(A, B) ->
    gen_server:call(?MODULE, {divides, A, B}).
