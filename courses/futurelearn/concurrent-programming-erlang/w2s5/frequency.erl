%% Based on code from
%%   Erlang Programming
%%   Francecso Cesarini and Simon Thompson
%%   O'Reilly, 2008
%%   http://oreilly.com/catalog/9780596518189/
%%   http://www.erlangprogramming.org/
%%   (c) Francesco Cesarini and Simon Thompson
-module(frequency).

-export([start/1, init/1, stop/0, allocate/0, deallocate/1, clear/0]).

-type frequency() :: integer().
-type state() :: {Free :: [frequency()], Allocated :: [{pid(), frequency()}]}. 
-type option() :: [{delay_send, non_neg_integer()}].


%% ===================================================================
%% API functions
%% ===================================================================

-spec start(option()) -> {ok, pid()}.
start(Opts) ->
    Pid = spawn(frequency, init, [Opts]),
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

-spec clear() -> ok.
clear() ->
    receive
        _Msg ->
            clear()
    after 0 ->
        ok
    end.

%% ===================================================================
%% Server functions
%% ===================================================================

-spec init(option()) -> {reply, stopped}.
init(Opts) ->
    Frequencies = get_frequencies(),
    loop({Frequencies, []}, Opts).

-spec loop(state(), option()) -> {reply, stopped}.
loop(Frequencies, Opts) ->
    DelaySend = proplists:get_value(delay_send, Opts, 0),
    if
        DelaySend >= 1 ->
            Sleep = rand:uniform(DelaySend),
            io:format("Simulate server overloaded with sleep: ~w...~n", [Sleep]),
            timer:sleep(Sleep); % Simulate server overloaded
        true ->
            false
    end,
    receive
      {request, Pid, allocate} ->
          {NewFrequencies, Reply} = handle_allocate(Frequencies, Pid),
          Pid ! {reply, Reply},
          loop(NewFrequencies, Opts);
      {request, Pid, {deallocate, Freq}} ->
          NewFrequencies = handle_deallocate(Frequencies, Freq),
          Pid ! {reply, ok},
          loop(NewFrequencies, Opts);
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

-spec get_reply() -> any().
get_reply() ->
    receive
        {reply, Reply} ->
            Reply
    after 100 ->
        clear(),
        io:format("Server overloaded: ~w!~n", [time()]),
        {error, server_overloaded}
    end.