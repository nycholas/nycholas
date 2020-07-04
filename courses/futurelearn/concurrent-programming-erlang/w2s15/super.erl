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
-module(super).

-export([start_link/0, stop/0]).
-export([init/0]).

-type state() :: [{atom(), pid()}].

%% ===================================================================
%% API functions
%% ===================================================================

-spec start_link() -> {ok, pid()}.
start_link() ->
    Pid = spawn_link(super, init, []),
    register(super, Pid),
    {ok, Pid}.

-spec stop() -> ok.
stop() ->
    super ! stop,
    unregister(super),
    ok.

%% ===================================================================
%% Server functions
%% ===================================================================

-spec init() -> ok.
init() ->
    process_flag(trap_exit, true),
    {ok, EchoPid} = echo_init(),
    {ok, TalkPid} = talk_init(),
    loop([{echo, EchoPid}, {talk, TalkPid}]).

%% ===================================================================
%% Private functions
%% ===================================================================

-spec loop(state()) -> ok.
loop(State) ->
    TalkPid = proplists:get_value(talk, State),
    EchoPid = proplists:get_value(echo, State),
    receive
      stop ->
          talk:stop(TalkPid),
          echo:stop(EchoPid),
          ok;
      {'EXIT', EchoPid, Reason} ->
          io:format("echo worker died: ~w. restart now...~n", [Reason]),
          {ok, NewEchoPid} = echo_init(),
          NewState = [{echo, NewEchoPid}, {talk, TalkPid}],
          loop(NewState);
      {'EXIT', TalkPid, Reason} ->
          io:format("talk worker died: ~w. restart now...~n", [Reason]),
          {ok, NewTalkPid} = talk_init(),
          NewState = [{echo, EchoPid}, {talk, NewTalkPid}],
          loop(NewState);
      _ ->
          loop(State)
    end.

-spec echo_init() -> {ok, pid()}.
echo_init() ->
    {ok, EchoPid} = echo:start_link(),
    ensure_register(echo, EchoPid),
    {ok, EchoPid}.

-spec talk_init() -> {ok, pid()}.
talk_init() ->
    {ok, TalkPid} = talk:start_link(),
    ensure_register(talk, TalkPid),
    {ok, TalkPid}.

-spec ensure_register(atom(), pid()) -> ok.
ensure_register(Name, Pid) ->
    case whereis(Name) of
      undefined ->
          ok;
      _ ->
          unregister(Name)
    end,
    register(Name, Pid),
    ok.

