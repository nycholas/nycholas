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
-module(frequency).

-behaviour(gen_server).

%% API functions
-export([start/0, stop/0, new/1, allocate/0, deallocate/1, inject/1, report/0]).

%% Generic server functions
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-type frequency() :: integer().
-type state() :: {Free :: [frequency()], Allocated :: [{pid(), frequency()}]}.

-define(SERVER, ?MODULE).

%% ===================================================================
%% API functions
%% ===================================================================

-spec start() -> {ok, pid()}.
start() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).
    
-spec stop() -> ok.
stop() ->
    gen_server:stop(?SERVER).

-spec new(frequency()) -> {ok, frequency()} | {error, atom()}.
new(Freq) ->
    gen_server:call(?SERVER, {new, Freq}).

-spec allocate() -> {ok, frequency()} | {error, atom()}.
allocate() ->
    gen_server:call(?SERVER, {allocate, self()}).
    
-spec deallocate(frequency()) -> ok.
deallocate(Freq) ->
    gen_server:cast(?SERVER, {deallocate, Freq}).
    
-spec inject([frequency()]) -> ok.
inject(Freqs) ->
    gen_server:cast(?SERVER, {inject, Freqs}).

-spec report() -> state().
report() ->
    gen_server:call(?SERVER, {report}).

%% ===================================================================
%% Generic server functions
%% ===================================================================

-spec init([]) -> {ok, state()}.
init(_Args) ->
    process_flag(trap_exit, true),
    Frequencies = get_frequencies(),
    {ok, {Frequencies, []}}.

handle_call({new, Freq}, _From, State) ->
    {NewState, Reply} = handle_new(State, Freq),
    {reply, Reply, NewState};
handle_call({allocate, Pid}, _From, State) ->
    {NewState, Reply} = handle_allocate(State, Pid),
    {reply, Reply, NewState};
handle_call({report}, _From, State) ->
    {reply, State, State};
handle_call(_Request, _From, State) ->
    {reply, ok, State}.

handle_cast({deallocate, Freq}, State) ->
    NewState = handle_deallocate(State, Freq),
    {noreply, NewState};    
handle_cast({inject, Freqs}, State) ->
    NewState = handle_inject(State, Freqs),
    {noreply, NewState};
handle_cast(_Request, State) ->
    {noreply, State}.
    
handle_info({'EXIT', _Pid, normal}, State) ->
    {noreply, State};
handle_info({'EXIT', _Pid, Reason}, State) ->
    io:format("worker died by reason: ~w.~n", [Reason]),
    {noreply, State};
handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% ===================================================================
%% Private functions
%% ===================================================================

-spec handle_new(state(), frequency()) -> {state(), {ok, frequency()} | {error, atom()}}.
handle_new({Free, Allocated}, Freq) ->
    AllFreqs = Free ++ [F || {F, _} <- Allocated],
    case lists:member(Freq, AllFreqs) of
      true ->
          {{Free, Allocated}, {error, already_frequency}};
      _ ->
          {{Free ++ [Freq], Allocated}, {ok, Freq}}
    end.

-spec handle_allocate(state(), pid()) -> {state(), {ok, frequency()} | {error, atom()}}.
handle_allocate({[], Allocated}, _Pid) ->
    {{[], Allocated}, {error, no_frequency}};
handle_allocate({[Freq | Free], Allocated}, Pid) ->
    case lists:keymember(Pid, 2, Allocated) of
      true ->
          {{[Freq | Free], Allocated}, {error, already_allocated}};
      _ ->
          {{Free, [{Freq, Pid} | Allocated]}, {ok, Freq}}
    end.

-spec handle_deallocate(state(), frequency()) -> state().
handle_deallocate({Free, Allocated}, Freq) ->
    case lists:keydelete(Freq, 1, Allocated) of
      Allocated ->
          {Free, Allocated};
      NewAllocated ->
          {[Freq | Free], NewAllocated}
    end.

-spec handle_inject(state(), [frequency()]) -> state().
handle_inject({Free, Allocated}, Freqs) ->
    {Free ++ Freqs, Allocated}.

-spec get_frequencies() -> [frequency()].
get_frequencies() ->
    lists:seq(10, 15).