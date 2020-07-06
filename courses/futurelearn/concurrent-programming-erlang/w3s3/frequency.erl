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

-spec init() -> ok.
init() ->
    Frequencies = get_frequencies(),
    loop({Frequencies, []}).

-spec loop(state()) -> ok.
loop(State) ->
    try handle_loop(State) of
      Reply ->
          Reply
    catch
      {unexpected_message, NewState} ->
          loop(NewState)
    end.

-spec handle_loop(state()) -> ok.
handle_loop(State) ->
    receive
      {request, Pid, allocate} ->
          {NewState, Reply} = handle_allocate(State, Pid),
          Pid ! {reply, Reply},
          loop(NewState);
      {request, Pid, {deallocate, Freq}} ->
          S = try handle_deallocate(State, Freq) of
                NewState ->
                    Pid ! {reply, ok},
                    NewState
              catch
                Throw ->
                    Pid ! {reply, {error, Throw}},
                    State
              end,
          loop(S);
      {request, Pid, stop} ->
          Pid ! {reply, stopped},
          ok;
      Unexpected ->
          io:format("Unxpected message: ~w~n", [Unexpected]),
          throw({unexpected_message, State})
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
          throw(unallocated_frequency);
      NewAllocated ->
          {[Freq | Free], NewAllocated}
    end.

-spec get_frequencies() -> [frequency()].
get_frequencies() ->
    [10, 11, 12, 13, 14, 15].

