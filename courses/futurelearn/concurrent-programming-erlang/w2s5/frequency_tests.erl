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

%% > [c(M) || M <- [frequency, frequency_tests]].
%% > eunit:test(frequency).
-include_lib("eunit/include/eunit.hrl").

-spec allocate_and_deallocate_test() -> ok.
allocate_and_deallocate_test() ->
    {ok, _Server} = frequency:start([]),
    {ok, Freq} = frequency:allocate(),
    ok = frequency:deallocate(Freq),
    stopped = frequency:stop(),
    ok.

%% ===================================================================
%% Tests descriptions
%% ===================================================================

simulate_server_overloaded_test_() -> 
    {"Simulate server overloaded",
     {setup,
     fun start/0,
     fun stop/1,
     fun simulate_server_overloaded/1}}.

%% ===================================================================
%% Setup functions
%% ===================================================================

start() ->
    {ok, Pid} = frequency:start([{delay_send, 200}]),
    Pid.

stop(_Pid) ->
    frequency:stop(),
    ok.

%% ===================================================================
%% Actual tests
%% ===================================================================

simulate_server_overloaded(_Pid) ->
    _ = [[{
            frequency:allocate(),
            frequency:deallocate(N)
        } || N <- [10, 11, 12, 13, 14, 15]] || _ <- lists:seq(0, 10)],
    [?_assertEqual(true, true)].
        
%% ===================================================================
%% Helper functions
%% ===================================================================