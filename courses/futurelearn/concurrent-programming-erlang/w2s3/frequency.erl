%% Based on code from
%%   Erlang Programming
%%   Francecso Cesarini and Simon Thompson
%%   O'Reilly, 2008
%%   http://oreilly.com/catalog/9780596518189/
%%   http://www.erlangprogramming.org/
%%   (c) Francesco Cesarini and Simon Thompson
-module(frequency).

-export([start/0, init/0, stop/1, allocate/1, deallocate/2]).

-type frequency() :: integer().
-type state() :: {Free :: [frequency()], Allocated :: [{pid(), frequency()}]}. 

%% ===================================================================
%% API functions
%% ===================================================================

-spec start() -> {ok, pid()}.
start() ->
    Pid = spawn(frequency, init, []),
    register(frequency, Pid),
    {ok, Pid}.

-spec stop(pid()) -> ok.
stop(From) ->
    frequency ! {request, From, stop},
    unregister(frequency),
    ok.

-spec allocate(pid()) -> ok.
allocate(From) ->
    frequency ! {request, From, allocate},
    ok.

-spec deallocate(pid(), integer()) -> ok.
deallocate(From, Freq) ->
    frequency ! {request, From, {deallocate, Freq}},
    ok.

%% ===================================================================
%% Server functions
%% ===================================================================

-spec init() -> {reply, stopped}.
init() ->
    Frequencies = get_frequencies(),
    loop({Frequencies, []}).

-spec loop(state()) -> {reply, stopped}.
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

-spec handle_allocate(state(), pid()) -> {state(), {ok | error, any()}}.
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
    case lists:keymember(Freq, 1, Allocated) of
      true ->
          NewAllocated = lists:keydelete(Freq, 1, Allocated),
          {[Freq | Free], NewAllocated};
      _ ->
          {Free, Allocated}
    end.

-spec get_frequencies() -> [frequency()].
get_frequencies() ->
    [10, 11, 12, 13, 14, 15].

