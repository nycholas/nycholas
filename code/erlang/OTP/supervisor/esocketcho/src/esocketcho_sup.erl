-module(esocketcho_sup).
-behaviour(supervisor).

-export([start_link/0, start_socket/0]).
-export([init/1]).

%% Publics
start_socket() ->
    supervisor:start_child(?MODULE, []).

%% Behaviours
start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->
    {ok, MaxRestart} = application:get_env(max_restart),
    {ok, MaxTime} = application:get_env(max_time),
    {ok, Port} = application:get_env(port),
    {ok, ListenSocket} = gen_tcp:listen(Port, [{active, once}, 
                                               {packet, line}]),
    spawn_link(fun empty_listeners/0),
    {ok, {{simple_one_for_one, MaxRestart, MaxTime},
     [{socket,
       {esocketcho_serv, start_link, [ListenSocket]},
        temporary, 1000, worker, [esocketcho_serv]}]}}.

%% Privates
empty_listeners() ->
    [start_socket() || _ <- lists:seq(1, 20)],
    ok.
