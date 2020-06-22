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
-module(server).

-export([server/1, server/0, balancer/1]).

-spec server(pid()) -> ok.
server(From) ->
    receive
        stop ->
            io:format("Stopped: server(~w)~n", [self()]),
            ok;
        {check, Msg} ->
            io:format("Received message: from(~w) on server(~w)~n", [From, self()]),
            case palin:palindrome(Msg) of
                true -> 
                    From ! {result, "\"" ++ Msg ++ "\" is a palindrome"};
                false ->
                    From ! {result, "\"" ++ Msg ++ "\" is not a palindrome"}
            end,
            server(From);
        _ ->
            io:format("Unexpect message: server(~w)~n", [self()]),
            server(From)
    end.

-spec server() -> ok.
server() ->
    receive
        stop ->
            io:format("Stopped: server(~w)~n", [self()]),
            ok;
        {check, From, Msg} ->
            io:format("Received message: from(~w) on server(~w)~n", [From, self()]),
            case palin:palindrome(Msg) of
                true -> 
                    From ! {result, "\"" ++ Msg ++ "\" is a palindrome"};
                false ->
                    From ! {result, "\"" ++ Msg ++ "\" is not a palindrome"}
            end,
            server();
        _ ->
            io:format("Unexpect message: server(~w)~n", [self()]),
            server()
    end.

-spec balancer(list(pid())) -> ok.
balancer([Server | Servers]) ->
    balancer(Server, Servers).

-spec balancer(pid(), list(pid())) -> ok.
balancer(Server, [Next | Servers]) ->
    receive
        stop ->
            io:format("Stopped: balancer(~w)~n", [self()]),
            [S ! stop || S <- Servers ++ [Server, Next]],
            ok;
        Message ->
            io:format("Received message: balancer(~w) executing by server(~w)~n", [self(), Server]),
            Server ! Message,
            balancer(Next, Servers ++ [Server])
    end.