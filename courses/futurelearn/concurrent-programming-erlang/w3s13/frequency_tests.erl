% Copyright (c) 2020, Nycholas de Oliveira e Oliveira <nycholas@gmail.com>
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
-module(frequency_tests).

%% > [c(M, [debug_info, {d, 'TEST', 1}]) || M <- [frequency, frequency_server, frequency_tests]].
%% > eunit:test(frequency).
-include_lib("eunit/include/eunit.hrl").

%% ===================================================================
%% Tests descriptions
%% ===================================================================

-spec simple_test_() -> {string(), {setup, fun(), fun(), fun()}}.
simple_test_() ->
    {"Simple server functions", {setup, fun start/0, fun stop/1, fun simple_test/1}}.

-spec info_servers_test_() -> {string(), {setup, fun(), fun(), fun()}}.
info_servers_test_() ->
    {"Get server info", {setup, fun start/0, fun stop/1, fun info_servers_test/1}}.

-spec info_for_one_poll_servers_test_() -> {string(), {setup, fun(), fun(), fun()}}.
info_for_one_poll_servers_test_() ->
    {"Get server info for one poll servers",
     {setup,
      fun () ->
              {ok, Pid} = frequency:start_link(),
              Pid
      end,
      fun stop/1,
      fun info_for_one_poll_servers_test/1}}.

-spec restart_work_when_died_test_() -> {string(), {setup, fun(), fun(), fun()}}.
restart_work_when_died_test_() ->
    {"Restart work when died",
     {setup,
      fun start/0,
      fun stop/1,
      fun restart_work_when_died_test/1}}.

%% ===================================================================
%% Setup functions
%% ===================================================================

-spec start() -> pid().
start() ->
    {ok, Pid} = frequency:start_link([{poll_servers, 3}]),
    Pid.

-spec stop(pid()) -> ok.
stop(_Pid) ->
    frequency:stop(),
    ok.

%% ===================================================================
%% Actual tests
%% ===================================================================

simple_test(_Pid) ->
    _ = [[{frequency:allocate(), frequency:deallocate(N)} || N <- [10, 11, 12, 13, 14, 15]]
         || _ <- lists:seq(0, 10)],
    [?_assertEqual(true, true)].

info_servers_test(_Pid) ->
    [{N1, F1, _}, {N2, F2, _}, {N3, F3, _}] = frequency:info(),
    ok = frequency:inject([1, 2, 3]),
    [{M1, G1, _}, {M2, G2, _}, {M3, G3, _}] = frequency:info(),
    [?_assertEqual([{'frequency-1', [10, 11, 12, 13, 14, 25]},
                    {'frequency-2', [15, 16, 17, 18, 19]},
                    {'frequency-3', [20, 21, 22, 23, 24]}],
                   [{N1, F1}, {N2, F2}, {N3, F3}]),
     ?_assertEqual([{'frequency-1', [10, 11, 12, 13, 14, 25, 1]},
                    {'frequency-2', [15, 16, 17, 18, 19, 2]},
                    {'frequency-3', [20, 21, 22, 23, 24, 3]}],
                   [{M1, G1}, {M2, G2}, {M3, G3}])].

info_for_one_poll_servers_test(_Pid) ->
    [{N1, F1, _}] = frequency:info(),
    ok = frequency:inject([1, 2, 3]),
    [{N2, F2, _}] = frequency:info(),
    [?_assertEqual([{'frequency-1',
                     [10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22, 23, 24, 25]}],
                   [{N1, F1}]),
     ?_assertEqual([{'frequency-1',
                     [10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22, 23, 24, 25, 1, 2, 3]}],
                   [{N2, F2}])].

restart_work_when_died_test(_Pid) ->
    {ok, Freq1} = frequency:allocate(),
    ok = frequency:deallocate(Freq1),
    [{N1, F1, S1}, {N2, F2, _}, {N3, F3, _}] = frequency:info(),
    exit(S1, normal),
    {ok, Freq2} = frequency:allocate(),
    ok = frequency:deallocate(Freq2),
    ok = frequency:inject([1, 2, 3, 4, 5, 6]),
    [{M1, G1, _}, {M2, G2, _}, {M3, G3, _}] = frequency:info(),
    [?_assertEqual([{'frequency-2', [15, 16, 17, 18, 19]},
                    {'frequency-3', [20, 21, 22, 23, 24]},
                    {'frequency-1', [10, 11, 12, 13, 14, 25]}],
                   [{N1, F1}, {N2, F2}, {N3, F3}]),
     ?_assertEqual([{'frequency-3', [20, 21, 22, 23, 24, 1, 2]},
                    {'frequency-1', [10, 11, 12, 13, 14, 25, 3, 4]},
                    {'frequency-2', [15, 16, 17, 18, 19, 5, 6]}],
                   [{M1, G1}, {M2, G2}, {M3, G3}])].

%% ===================================================================
%% Helper functions
%% ===================================================================
