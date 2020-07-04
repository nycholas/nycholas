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
-module(echo).

-export([start_link/0, stop/1]).
-export([init/0, handle_cast/2]).

-type state() :: [].

%% ===================================================================
%% API functions
%% ===================================================================

-spec start_link() -> {ok, pid()}.
start_link() ->
    Pid = spawn_link(echo, init, []),
    {ok, Pid}.

-spec stop(pid()) -> ok.
stop(Pid) ->
    Pid ! stop,
    ok.

%% ===================================================================
%% Server functions
%% ===================================================================

-spec init() -> ok.
init() ->
    loop([]).

-spec handle_cast(any(), pid()) -> ok.
handle_cast({message, Ref, Msg}, From) ->
    echo ! {{message, Ref, Msg}, From},
    ok;
handle_cast(Msg, From) ->
    io:format("unexpected message \"~w\" from ~w.~n", [Msg, From]),
    ok.

%% ===================================================================
%% Private functions
%% ===================================================================

-spec loop(state()) -> ok.
loop(State) ->
    receive
        {{message, Ref, N} = Msg, From} ->
            io:format("~w echoed.~n", [Msg]),
            From ! {reply, {message, Ref, N}},
            loop(State);
        stop ->
            io:format("stopped echo worker.~n", []),
            ok;
        {exit, Reason} ->
            io:format("exit talk with reason ~w.~n", [Reason]),
            exit(Reason)
    end.
