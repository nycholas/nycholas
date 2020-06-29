%% Based on code from
%%   Erlang Programming
%%   Francecso Cesarini and Simon Thompson
%%   O'Reilly, 2008
%%   http://oreilly.com/catalog/9780596518189/
%%   http://www.erlangprogramming.org/
%%   (c) Francesco Cesarini and Simon Thompson
-module(frequency).

-export([start/0, init/0, stop/1, allocate/1, deallocate/2]).

%% ===================================================================
%% API functions
%% ===================================================================

start() ->
    Pid = spawn(frequency, init, []),
    register(frequency, Pid),
    {ok, Pid}.

stop(From) ->
    frequency ! {request, From, stop},
    unregister(frequency),
    ok.

allocate(From) ->
    frequency ! {request, From, allocate},
    ok.

deallocate(From, Freq) ->
    frequency ! {request, From, {deallocate, Freq}},
    ok.

%% ===================================================================
%% Server functions
%% ===================================================================

init() ->
    Frequencies = get_frequencies(),
    loop({Frequencies, []}).

loop(Frequencies) ->
    receive
      {request, Pid, allocate} ->
          {NewFrequencies, Reply} = handle_allocate(Frequencies, Pid),
          Pid ! {reply, Reply},
          loop(NewFrequencies);
      {request, Pid, {deallocate, Freq}} ->
          NewFrequencies = handle_deallocate(Frequencies, Freq),
          Pid ! {reply, ok},
          loop(NewFrequencies);
      {request, Pid, stop} ->
          Pid ! {reply, stopped}
    end.

%% ===================================================================
%% Private functions
%% ===================================================================

handle_allocate({[], Allocated}, _Pid) ->
    {{[], Allocated}, {error, no_frequency}};
handle_allocate({[Freq | Free], Allocated}, Pid) ->
    case lists:keymember(Pid, 2, Allocated) of
      true ->
          {{[Freq | Free], Allocated}, {error, already_allocated}};
      _ ->
          {{Free, [{Freq, Pid} | Allocated]}, {ok, Freq}}
    end.

handle_deallocate({Free, Allocated}, Freq) ->
    case lists:keymember(Freq, 1, Allocated) of
      true ->
          NewAllocated = lists:keydelete(Freq, 1, Allocated),
          {[Freq | Free], NewAllocated};
      _ ->
          {Free, Allocated}
    end.

get_frequencies() ->
    [10, 11, 12, 13, 14, 15].

