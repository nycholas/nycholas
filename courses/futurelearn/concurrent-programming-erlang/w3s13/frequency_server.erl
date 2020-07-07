%% Based on code from
%%   Erlang Programming
%%   Francecso Cesarini and Simon Thompson
%%   O'Reilly, 2008
%%   http://oreilly.com/catalog/9780596518189/
%%   http://www.erlangprogramming.org/
%%   (c) Francesco Cesarini and Simon Thompson
-module(frequency_server).

-export([start_link/2, stop/1, allocate/1, deallocate/2, inject/2]).
-export([init/2, loop/2]).

-type frequency() :: integer().
-type state() :: {Free :: [frequency()], Allocated :: [{pid(), frequency()}]}.

%% ===================================================================
%% API functions
%% ===================================================================

-spec start_link(atom(), [frequency()]) -> {ok, pid()}.
start_link(Name, Frequencies) ->
    Pid = spawn_link(frequency_server, init, [Name, Frequencies]),
    register(Name, Pid),
    {ok, Pid}.

-spec stop(pid()) -> ok.
stop(Pid) ->
    Pid ! {request, self(), stop},
    get_reply().

-spec allocate(pid()) -> {ok | error, any()}.
allocate(Pid) ->
    Pid ! {request, self(), allocate},
    get_reply().

-spec deallocate(pid(), integer()) -> ok | {error, any()}.
deallocate(Pid, Freq) ->
    Pid ! {request, self(), {deallocate, Freq}},
    get_reply().

-spec inject(pid(), [frequency()]) -> ok.
inject(Pid, Freqs) ->
    Pid ! {request, self(), {inject, Freqs}},
    get_reply().

%% ===================================================================
%% Server functions
%% ===================================================================

-spec init(atom(), [frequency()]) -> ok.
init(Name, Frequencies) ->
    loop(Name, {Frequencies, []}).

-spec loop(atom(), state()) -> ok.
loop(Name, State) ->
    try handle_loop(Name, State) of
      Reply ->
          Reply
    catch
      _:{_, NewState} ->
          ?MODULE:loop(Name, NewState)
    end.

%% ===================================================================
%% Private functions
%% ===================================================================

-spec handle_loop(atom(), state()) -> ok.
handle_loop(Name, State) ->
    receive
      {request, From, allocate} ->
          {NewState, Reply} = handle_allocate(State, From),
          From ! {reply, Reply},
          ?MODULE:loop(Name, NewState);
      {request, From, {deallocate, Freq}} ->
          S = try handle_deallocate(State, Freq) of
                NewState ->
                    From ! {reply, ok},
                    NewState
              catch
                Throw ->
                    From ! {reply, {error, Throw}},
                    State
              end,
          ?MODULE:loop(Name, S);
      {request, From, {inject, Freqs}} ->
          NewState = handle_inject(State, Freqs),
          From ! {reply, ok},
          ?MODULE:loop(Name, NewState);
      {request, From, stop} ->
          From ! {reply, ok},
          ok;
      _ ->
          ?MODULE:loop(Name, State)
    end.

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

-spec handle_inject(state(), [frequency()]) -> state().
handle_inject({Free, Allocated}, Freqs) ->
    {Free ++ Freqs, Allocated}.

-spec get_reply() -> any() | {error, timeouted}.
get_reply() ->
    receive
      {reply, Reply} ->
          Reply
      after 500 ->
                {error, timeouted}
    end.

