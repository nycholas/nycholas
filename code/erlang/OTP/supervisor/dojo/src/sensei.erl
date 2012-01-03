-module(sensei).
-behaviour(supervisor).

-export([start_link/1]).
-export([init/1]).

start_link(Type) ->
    supervisor:start_link({local,?MODULE}, ?MODULE, Type).

init(kame) ->
    init({one_for_one, 3, 60000});
init(seijuro_hiko) ->
    init({rest_for_one, 2, 60000});
init(pai_mei) ->
    init({one_for_all, 1, 60000});
init({RestartStrategy, MaxRestart, MaxTime}) ->
    {ok, {{RestartStrategy, MaxRestart, MaxTime},
         [{aikido,
           {fighters, start_link, [aikido, good]},
           permanent, 1000, worker, [fighters]},
          {box,
           {fighters, start_link, [box, bad]},
           temporary, 1000, worker, [fighters]},
          {judo,
           {fighters, start_link, [judo, good]},
           transient, 1000, worker, [fighters]},
          {kung_fu,
           {fighters, start_link, [kung_fu, good]},
           permanent, 1000, worker, [fighters]},
          {jiu_jitsu,
           {fighters, start_link, [jiu_jitsu, bad]},
           permanent, 1000, worker, [fighters]},
          {karate,
           {fighters, start_link, [karate, good]},
           transient, 1000, worker, [fighters]},
          {samurai,
           {fighters, start_link, [samurai, good]},
           permanent, 1000, worker, [fighters]}
         ]}}.

