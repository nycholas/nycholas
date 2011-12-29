-module(zombie_fsm).
-export([start/0, victim/1, shotgun/1]).

start() ->
    spawn(fun() -> seek() end).

victim(Pid) -> 
    Pid ! victim.

shotgun(Pid) -> 
    Pid ! shotgun.

seek() ->
    io:format("Zombie is looking for food~n"),
    receive
        victim ->
            io:format("Zombie caught the smell of fresh meat~n"),
            search();
        _ ->
            io:format("Zombie is lost!~n"),
            seek()
    after 2000 ->
        seek()
    end.

search() -> 
    io:format("Zombie food found~n"),
    receive
         shotgun ->
             io:format("Zombie is shot in the head and faints...~n"),
             seek();
         _ ->
             io:format("Nothing can stop the Zombie!~n"),
             search()
    after 30000 ->
        destroy()
    end.

destroy() ->
    io:format("Trips and the victim Zombie eats...~n"),
    receive
        _ -> 
            io:format("Something stopped the Zombie~n"),
            seek()
    after 10000 ->
        seek()
    end.

