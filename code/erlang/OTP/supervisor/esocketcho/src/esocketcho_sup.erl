% Simple example server.
% Copyright (c) 2012, Nycholas de Oliveira e Oliveira <nycholas@gmail.com>
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
