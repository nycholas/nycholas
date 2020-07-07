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

-export([start_link/0, start_link/1, stop/0, info/0, allocate/0, deallocate/1, inject/1]).
-export([init/1, loop/1]).

-type state() :: [{Name :: atom(),
                   Frequencies :: frequency_server:frequency(),
                   Pid :: pid()}].
-type option() :: [{atom(), any()}].

%% ===================================================================
%% API functions
%% ===================================================================

-spec start_link() -> {ok, pid()}.
start_link() ->
    start_link([]).

-spec start_link(option()) -> {ok, pid()}.
start_link(Opts) ->
    Pid = spawn_link(frequency, init, [Opts]),
    register(frequency, Pid),
    {ok, Pid}.

-spec stop() -> ok.
stop() ->
    frequency ! {request, self(), stop},
    get_reply().

-spec info() -> state().
info() ->
    frequency ! {request, self(), info},
    get_reply().

-spec allocate() -> {ok | error, any()}.
allocate() ->
    frequency ! {request, self(), allocate},
    get_reply().

-spec deallocate(frequency_server:frequency()) -> ok | {error, any()}.
deallocate(Freq) ->
    frequency ! {request, self(), {deallocate, Freq}},
    get_reply().

-spec inject([frequency_server:frequency()]) -> ok.
inject(Freqs) ->
    frequency ! {request, self(), {inject, Freqs}},
    get_reply().

%% ===================================================================
%% Server functions
%% ===================================================================

-spec init(option()) -> ok.
init(Opts) ->
    process_flag(trap_exit, true),
    PollServers = proplists:get_value(poll_servers, Opts, 0),
    ServersFrequencies = [{frequency_server_name(I), F}
                          || {I, F} <- frequencies_by(PollServers, get_frequencies())],
    State = [{N, F, P}
             || {N, F, {ok, P}}
                    <- [{N, F, frequency_server:start_link(N, F)} || {N, F} <- ServersFrequencies]],
    loop(State).

-spec loop(state()) -> ok.
loop(State) ->
    receive
      {request, From, stop} ->
          _ = [{unregister(N), frequency_server:stop(S)} || {N, _F, S} <- State],
          From ! {reply, ok},
          ok;
      {request, From, info} ->
          From ! {reply, State},
          ?MODULE:loop(State);
      {request, From, allocate} ->
          Reply = handle_allocate(State),
          From ! {reply, Reply},
          [H | T] = State,
          ?MODULE:loop(T ++ [H]);
      {request, From, {deallocate, Freq}} ->
          Reply = handle_deallocate(State, Freq),
          From ! {reply, Reply},
          ?MODULE:loop(State);
      {request, From, {inject, Freqs}} ->
          NewState = handle_inject(State, Freqs),
          From ! {reply, ok},
          ?MODULE:loop(NewState);
      {'EXIT', Pid, Reason} ->
          io:format("worker died by reason: ~w. trying restart now...~n", [Reason]),
          Partition = lists:partition(fun ({_N, _F, S}) ->
                                              S == Pid
                                      end,
                                      State),
          case Partition of
            {[], State} ->
                io:format("worker \"~w\" not found, impossible for restart.~n", [Pid]),
                ?MODULE:loop(State);
            {[{N, F, Pid}], Part} ->
                io:format("restart worker \"~w\" with: ~w.~n", [N, F]),
                NewState = Part ++ [{N, F, frequency_server:start_link(N, F)}],
                ?MODULE:loop(NewState)
          end;
      _ ->
          ?MODULE:loop(State)
    end.

%% ===================================================================
%% Private functions
%% ===================================================================

-spec frequencies_by(non_neg_integer(),
                     [frequency_server:frequency()]) -> [{non_neg_integer(),
                                                          [frequency_server:frequency()]}].
frequencies_by(N, Frequencies) when N > 0 ->
    Len = length(Frequencies),
    Parts = Len div N,
    List = [lists:sublist(Frequencies, X, Parts)
            || X <- lists:seq(1, length(Frequencies), Parts)],
    Tuples = [{X, lists:nth(X, List)} || X <- lists:seq(1, length(List))],
    case length(Tuples) > N of
      true ->
          {_, Last} = lists:last(Tuples),
          [{I, F} | SubList] = lists:sublist(Tuples, 1, length(Tuples) - 1),
          [{I, F ++ Last} | SubList];
      false ->
          Tuples
    end;
frequencies_by(_N, Frequencies) ->
    [{1, Frequencies}].

-spec get_frequencies() -> [frequency_server:frequency()].
get_frequencies() ->
    lists:seq(10, 25).

-spec frequency_server_name(non_neg_integer()) -> atom().
frequency_server_name(N) when is_integer(N) ->
    list_to_atom("frequency-" ++ integer_to_list(N)).

-spec get_reply() -> any() | {error, timeouted}.
get_reply() ->
    receive
      {reply, Reply} ->
          Reply
      after 500 ->
                {error, timeouted}
    end.

-spec handle_allocate(state()) -> {ok | error, any()}.
handle_allocate([]) ->
    {error, no_frequency};
handle_allocate([{_N, _F, S} | Servers]) ->
    case frequency_server:allocate(S) of
      {error, no_frequency} ->
          handle_allocate(Servers);
      {error, Reason} ->
          {error, Reason};
      {ok, Freq} ->
          {ok, Freq}
    end.

-spec handle_deallocate(state(), frequency_server:frequency()) -> {ok | error, any()}.
handle_deallocate(Servers, Freq) ->
    Server = [S || {_N, F, S} <- Servers, lists:member(Freq, F)],
    case Server of
      [] ->
          {error, frequency_doesnt_exists};
      [S | _] ->
          frequency_server:deallocate(S, Freq)
    end.

-spec handle_inject(state(), [frequency_server:frequency()]) -> state().
handle_inject(Servers, Freqs) ->
    Len = length(Servers),
    SplitFrequencies = frequencies_by(Len, Freqs),
    lists:map(fun ({{N, F1, S}, {_, F2}}) ->
                      frequency_server:inject(S, F2),
                      {N, F1 ++ F2, S}
              end,
              lists:zip(Servers, SplitFrequencies)).

%% ===================================================================
%% Tests functions
%% ===================================================================

-ifdef(TEST).

-include_lib("eunit/include/eunit.hrl").

-spec frequencies_by_test() -> ok.
frequencies_by_test() ->
    ?assertEqual([{1, [10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22, 23, 24, 25]}],
                 frequencies_by(0, get_frequencies())),
    ?assertEqual([{1, [10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22, 23, 24, 25]}],
                 frequencies_by(1, get_frequencies())),
    ?assertEqual([{1, [10, 11, 12, 13, 14, 15, 16, 17]},
                  {2, [18, 19, 20, 21, 22, 23, 24, 25]}],
                 frequencies_by(2, get_frequencies())),
    ?assertEqual([{1, [10, 11, 12, 13, 14, 25]},
                  {2, [15, 16, 17, 18, 19]},
                  {3, [20, 21, 22, 23, 24]}],
                 frequencies_by(3, get_frequencies())),
    ok.

-endif.

