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
-module(mailbox).

-export([start/0, stop/1, send/2, ordered_receiver/0]).

-spec start() -> ok.
start() ->
    receiver().

-spec stop(pid()) -> ok.
stop(Server) ->
    Server ! stop,
    ok.

-spec send(pid(), any()) -> ok.
send(Server, Msg) ->
    Server ! Msg,
    ok.

-spec receiver() -> ok.
receiver() ->
    receive
        stop ->
            io:format("stopped~n", []),
            ok;
        Msg ->
            io:format("message: ~w~n", [Msg]),
            timer:sleep(500),
            receiver()
    end.

-spec ordered_receiver() -> ok.
ordered_receiver() ->
    receive
        {first, FirstString} ->
            io:format("first message: ~w~n", [FirstString])
    end,
    receive
        {second, SecondString} ->
            io:format("second message: ~w~n", [SecondString])
    end,
    ok.