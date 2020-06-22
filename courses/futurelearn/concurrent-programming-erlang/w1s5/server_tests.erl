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
-module(server_tests).

%% > [c(M) || M <- [server, client, palin, server_tests]].
%% > eunit:test(server).
-include_lib("eunit/include/eunit.hrl").

-spec simple_test() -> ok.
simple_test() ->
    Self = self(),
    Server = spawn(server, server, [Self]),
    Server ! {check, "Madam I\'m Adam"},
    receive
        {resul, Result} ->
            io:format("Result: ~s~n", [Result]);
        _ ->
            ok
    end,
    Server ! stop,
    ok.

-spec multi_client_test() -> ok.
multi_client_test() ->
    Self = self(),
    Server = spawn(server, server, []),
    Server ! {check, Self, "Madam I\'m Adam"},
    receive
        {resul, Result} ->
            io:format("Result: ~s~n", [Result]);
        _ ->
            ok
    end,
    Server ! stop,
    ok.

-spec balancer_test() -> ok.
balancer_test() ->
    Self = self(),
    Server1 = spawn(server, server, []),
    Server2 = spawn(server, server, []),
    Server3 = spawn(server, server, []),
    Balancer = spawn(server, balancer, [[Server1, Server2, Server3]]),
    Balancer ! {check, Self, "Madam I\'m Adam"},
    receive
        {resul, Result} ->
            io:format("Result: ~s~n", [Result]);
        _ ->
            ok
    end,
    Balancer ! stop,
    ok.