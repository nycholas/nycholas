%% Based on code from
%%   Erlang Programming
%%   Francecso Cesarini and Simon Thompson
%%   O'Reilly, 2008
%%   http://oreilly.com/catalog/9780596518189/
%%   http://www.erlangprogramming.org/
%%   (c) Francesco Cesarini and Simon Thompson
-module(frequency).

-export([start/0, stop/0, allocate/0, deallocate/1, inject/1]).
-export([init/0, loop/1]).

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

-spec stop() -> stopped.
stop() ->
    frequency ! {request, self(), stop},
    unregister(frequency),
    get_reply().

-spec allocate() -> {ok | error, any()}.
allocate() ->
    frequency ! {request, self(), allocate},
    get_reply().

-spec deallocate(integer()) -> ok.
deallocate(Freq) ->
    frequency ! {request, self(), {deallocate, Freq}},
    get_reply().

-spec inject([frequency()]) -> ok.
inject(Freqs) ->
    frequency ! {request, self(), {inject, Freqs}},
    get_reply().

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
      {request, From, allocate} ->
          {NewFrequencies, Reply} = handle_allocate(Frequencies, From),
          From ! {reply, Reply},
          ?MODULE:loop(NewFrequencies);
      {request, From, {deallocate, Freq}} ->
          NewFrequencies = handle_deallocate(Frequencies, Freq),
          From ! {reply, ok},
          ?MODULE:loop(NewFrequencies);
      {request, From, {inject, Freqs}} ->
          NewFrequencies = handle_inject(Frequencies, Freqs),
          From ! {reply, ok},
          ?MODULE:loop(NewFrequencies);
      {request, From, stop} ->
          From ! {reply, stopped};
      _ ->
          ?MODULE:loop(Frequencies)
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
    case lists:keydelete(Freq, 1, Allocated) of
      Allocated ->
          Allocated;
      NewAllocated ->
          {[Freq | Free], NewAllocated}
    end.

-spec handle_inject(state(), [frequency()]) -> state().
handle_inject({Free, Allocated}, Freqs) ->
    {Free ++ Freqs, Allocated}.

-spec get_frequencies() -> [frequency()].
get_frequencies() ->
    [10, 11, 12, 13, 14, 15].

-spec get_reply() -> any().
get_reply() ->
    receive
      {reply, Reply} ->
          Reply
      after 500 ->
                {error, timeouted}
    end.

