#!/usr/bin/env escript
%% -*- coding: utf-8 -*-
%% -*- erlang -*-

main(_) ->
    process_flag(trap_exit, true),
    Port = open_port({spawn, "./calc.py"}, [{packet, 2}]),
    io:format("send: {~p, {command, [1, 10, 20]}}~n", [self()]),
    Port ! {self(), {command, [1, 10, 20]}},
    receive
        {Port, {data, Data}} ->
            io:format("received: {~p, {data, ~p}}~n", [Port, Data])
    end,
    port_close(Port).