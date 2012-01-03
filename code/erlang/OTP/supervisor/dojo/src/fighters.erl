-module(fighters).
-behaviour(gen_server).

-export([start_link/2, stop/1]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         code_change/3, terminate/2]).

-record(state, {name="", role, skill=good}).
-define(DELAY, 750).


%%% Public API
start_link(Role, Skill) ->
    gen_server:start_link({local, Role}, ?MODULE, [Role, Skill], []).

stop(Role) -> 
    gen_server:call(Role, stop).


%%% Server functions
init([Role, Skill]) ->
    %% To know when the parent shuts down
    process_flag(trap_exit, true),
    %% sets a seed for random number generation for the life of the process
    %% uses the current time to do it. Unique value guaranteed by now()
    random:seed(now()),
    TimeToPlay = random:uniform(3000),
    Name = pick_name(),
    StrRole = atom_to_list(Role),
    io:format("Fighter ~s ~s start training~n", [Name, StrRole]),
    {ok, #state{name=Name, role=StrRole, skill=Skill}, TimeToPlay}.

handle_call(stop, _From, S=#state{}) ->
    {stop, normal, ok, S};
handle_call(_Message, _From, S) ->
    {noreply, S, ?DELAY}.

handle_cast(_Message, S) ->
    {noreply, S, ?DELAY}.

handle_info(timeout, S=#state{name=N, skill=good}) ->
    case random:uniform(10) of
        6 -> 
            io:format("~s are tired~n", [N]),
            {stop, tired, S};
        _ ->
            io:format("~s nice blow!~n", [N]),
            {noreply, S, ?DELAY}
    end;
handle_info(timeout, S=#state{name=N, skill=bad}) ->
    case random:uniform(5) of
        1 ->
            io:format("~s lowers his guard and takes a punch~n", [N]),
            {stop, bad_struck, S};
        3 ->
            io:format("~s are tired~n", [N]),
            {stop, tired, S};
        _ ->
            io:format("~s stands for and tries to talk back~n", [N]),
            {noreply, S, ?DELAY}
    end;
handle_info(_Message, S) ->
    {noreply, S, ?DELAY}.

code_change(_OldVsm, State, _Extra) ->
    {ok, State}.

terminate(normal, S) ->
    io:format("~s (~s) winner the fight!~n", [S#state.name, S#state.role]);
terminate(bad_struck, S) ->
    io:format("~s falls~n", [S#state.name]);
terminate(tired, S) ->
    io:format("~s tired of training~n", [S#state.name]);
terminate(shutdown, S) ->
    io:format("End of training of the ~s!~n", [S#state.name]);
terminate(_Reason, S) ->
    io:format("~s has been kicked out (~s)~n", [S#state.name, S#state.role]).


%%% Private functions
pick_name() ->
    lists:nth(random:uniform(12), fighter_names()).

fighter_names() ->
    ["Ryu", "Ken", "Sagat", "Eagle", "Lee", "Joe", "Gen", "Geki",
     "Retsu", "Adon", "Mike", "Birdie"].

