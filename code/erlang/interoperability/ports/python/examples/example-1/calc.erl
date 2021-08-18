% Example using a port.
% Copyright (c) 2021, Nycholas de Oliveira e Oliveira <nycholas@gmail.com>
% All rights reserved.
%
% Redistribution and use in source and binary forms, with or without
% modification, are permitted provided that the following conditions are met:
%
% % Redistributions of source code must retain the above copyright notice,
%    this list of conditions and the following disclaimer.
% % Redistributions in binary form must reproduce the above copyright notice,
%    this list of conditions and the following disclaimer in the documentation
%    and/or other materials provided with the distribution.
% % Neither the name of the Nycholas de Oliveira e Oliveira nor the names of
%    its contributors may be used to endorse or promote products derived from
%    this software without specific prior written permission.
%
% THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS"
% AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
% IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
% ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE
% LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR
% CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF
% SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS
% INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN
% CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE)
% ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
% POSSIBILITY OF SUCH DAMAGE.
-module(calc).

%% API
-export([start/0, stop/0]).
-export([sum/2, subtract/2, multiplies/2, divides/2]).

%% Callbacks
-export([init/0]).

%% ===================================================================
%% API functions
%% ===================================================================

start() ->
    spawn(fun init/0).

stop() ->
    calc ! stop.

sum(A, B) -> 
    call_port({sum, A, B}).

subtract(A, B) -> 
    call_port({subtract, A, B}).

multiplies(A, B) -> 
    call_port({multiplies, A, B}).

divides(A, B) -> 
    call_port({divides, A, B}).

%% ===================================================================
%% Callbacks
%% ===================================================================

init() ->
    register(calc, self()),
    process_flag(trap_exit, true),
    Port = open_port({spawn, "./calc.py"}, [{packet, 2}]),
    loop(Port).

%% ===================================================================
%% Internal functions
%% ===================================================================

loop(Port) ->
    receive
        {call, Caller, Msg} ->
            Port ! {self(), {command, encode(Msg)}},
            receive
                {Port, {data, Data}} ->
                    Caller ! {calc, decode(Data)}
            end,
            loop(Port);
        stop ->
            Port ! {self(), close},
            receive
                {Port, closed} ->
                    exit(normal)
            end;
        {'EXIT', Port, Reason} ->
            exit({port_terminated, Reason})
    end.

call_port(Msg) ->
    calc ! {call, self(), Msg},
    receive
        {calc, Result} ->
            Result
    end.

encode({sum, A, B}) -> [1, A, B];
encode({subtract, A, B}) -> [2, A, B];
encode({multiplies, A, B}) -> [3, A, B];
encode({divides, A, B}) -> [4, A, B].

decode([_, Int]) -> Int.
