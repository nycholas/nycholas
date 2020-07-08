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

%% > [c(M, [debug_info, {d, 'TEST', 1}]) || M <- [frequency, frequency_tests]].
%% > eunit:test(frequency).
-include_lib("eunit/include/eunit.hrl").

-define(setup(F), {setup, fun start/0, fun stop/1, F}).

%% ===================================================================
%% Tests descriptions
%% ===================================================================

-spec simple_test_() -> {string(), {setup, fun(), fun(), fun()}}.
simple_test_() ->
    {"Simple server functions", ?setup(fun simple_test/1)}.

%% ===================================================================
%% Setup functions
%% ===================================================================

-spec start() -> pid().
start() ->
    {ok, Pid} = frequency:start(),
    Pid.

-spec stop(pid()) -> ok.
stop(_Pid) ->
    frequency:stop(),
    ok.

%% ===================================================================
%% Actual tests
%% ===================================================================

simple_test(_Pid) ->
    T1 = frequency:allocate(),
    T2 = frequency:allocate(),
    T3 = frequency:deallocate(10),
    T4 = frequency:deallocate(10),
    T5 = frequency:inject([1, 2, 3]),
    T6 = frequency:allocate(),
    {T71, [{T721, _}]} = frequency:report(),
    T8 = frequency:new(16),
    {T91, [{T921, _}]} = frequency:report(),
    [?_assertEqual({ok, 10}, T1),
     ?_assertEqual({error, already_allocated}, T2),
     ?_assertEqual(ok, T3),
     ?_assertEqual(ok, T4),
     ?_assertEqual(ok, T5),
     ?_assertEqual({ok, 10}, T6),
     ?_assertEqual({[11, 12, 13, 14, 15, 1, 2, 3], [10]}, {T71, [T721]}),
     ?_assertEqual({ok, 16}, T8),
     ?_assertEqual({[11, 12, 13, 14, 15, 1, 2, 3, 16], [10]}, {T91, [T921]})].


%% ===================================================================
%% Helper functions
%% ===================================================================
