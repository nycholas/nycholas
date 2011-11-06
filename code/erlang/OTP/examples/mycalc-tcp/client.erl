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
-module(client).

-revision('Revision: 0.1 ').
-created('Date: 2011/11/06 15:11:10 ').
-created_by('nycholas@gmail.com').
-modified('Date: 2011/11/06 15:11:10 ').
-modified_by('nycholas@gmail.com').

-export([send/1, sum/2, subtract/2, multiplies/2, divides/2]).

-define(TCP_OPTIONS, [binary, {packet, 0}]).

send(Str) ->
    case gen_tcp:connect("localhost", 7000, ?TCP_OPTIONS) of
        {ok, Socket} ->
            ok = gen_tcp:send(Socket, term_to_binary(Str)),
            wait_reply(Socket);
        Error -> 
            Error
    end.

sum(A, B) ->
    case gen_tcp:connect("localhost", 7000, ?TCP_OPTIONS) of
        {ok, Socket} ->
            ok = gen_tcp:send(Socket, term_to_binary({sum, A, B})),
            wait_reply(Socket);
        Error -> 
            Error
    end.

subtract(A, B) ->
    case gen_tcp:connect("localhost", 7000, ?TCP_OPTIONS) of
        {ok, Socket} ->
            ok = gen_tcp:send(Socket, term_to_binary({subtract, A, B})),
            wait_reply(Socket);
        Error -> 
            Error
    end.
    
multiplies(A, B) ->
    case gen_tcp:connect("localhost", 7000, ?TCP_OPTIONS) of
        {ok, Socket} ->
            ok = gen_tcp:send(Socket, term_to_binary({multiplies, A, B})),
            wait_reply(Socket);
        Error -> 
            Error
    end.
    
divides(A, B) ->
    case gen_tcp:connect("localhost", 7000, ?TCP_OPTIONS) of
        {ok, Socket} ->
            ok = gen_tcp:send(Socket, term_to_binary({divides, A, B})),
            wait_reply(Socket);
        Error -> 
            Error
    end.
    
wait_reply(Socket) ->
    receive
 	    {tcp, Socket, Bin} ->
 	        Term = binary_to_term(Bin),
 	        io:format("Client received binary = ~p~n", [Bin]),
            io:format("Client result = ~p~n", [Term]),
	        ok = gen_tcp:close(Socket),
	        Term;
 	    {tcp_closed, Socket} ->
	        true
      end.
