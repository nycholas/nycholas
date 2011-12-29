-module(mycalc_server).
-behaviour(gen_server).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).
-export([start/0, start_link/0, stop/1, sum/3, subtract/3, multiplies/3, divides/3]).

-record(operation, {name, a, b, result}).

start() -> 
    gen_server:start(?MODULE, [], []).

start_link() -> 
    gen_server:start_link(?MODULE, [], []).

stop(Pid) ->
    gen_server:call(Pid, terminate).

sum(Pid, A, B) ->
    gen_server:call(Pid, {sum, A, B}).

subtract(Pid, A, B) ->
    gen_server:call(Pid, {subtract, A, B}).

multiplies(Pid, A, B) ->
    gen_server:call(Pid, {multiplies, A, B}).

divides(Pid, A, B) ->
    gen_server:call(Pid, {divides, A, B}).


%%% Server functions
init([]) -> 
    {ok, []}. %% no treatment of info here!

handle_call({sum, A, B}, _From, State) ->
    NewState = make_operation(sum, A, B, Reply=A+B),
    {reply, Reply, [NewState|State]};

handle_call({subtract, A, B}, _From, State) ->
    NewState = make_operation(subtract, A, B, Reply=A-B),
    {reply, Reply, [NewState|State]};

handle_call({multiplies, A, B}, _From, State) ->
    NewState = make_operation(multiplies, A, B, Reply=A*B),
    {reply, Reply, [NewState|State]};

handle_call({divides, A, B}, _From, State) ->
    NewState = make_operation(divides, A, B, Reply=A/B),
    {reply, Reply, [NewState|State]};

handle_call(terminate, _From, State) ->
    {stop, normal, ok, State}.

handle_cast({return, Operation = #operation{}}, State) ->
    {noreply, [Operation|State]}.
    
handle_info(Msg, State) -> 
    io:format("Unexpected message: ~p~n", [Msg]),
    {noreply, State}.

terminate(normal, State) ->
    [io:format("~p(~p, ~p) = ~p, was set free.~n", 
     [S#operation.name, S#operation.a, S#operation.b, S#operation.result]) || S <- State],
    ok.

code_change(_OldVsn, State, _Extra) -> 
    %% No change planned. The function is there for the behaviour,
    %% but will not be used. Only a version on the next
    {ok, State}.


%%% Private functions
make_operation(Name, A, B, Result) ->
    #operation{name=Name, a=A, b=B, result=Result}.
