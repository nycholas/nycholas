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
    From = self(),
    {ok, _Server} = frequency:start(),
    ok = frequency:allocate(From),
    {ok, Freq} = get_reply(),
    ok = frequency:deallocate(From, Freq),
    ok = get_reply(),
    ok = frequency:stop(From),
    stopped = get_reply(),
    ok.

-spec allocate_more_than_one_test() -> ok.
allocate_more_than_one_test() ->
    From = self(),
    {ok, _Server} = frequency:start(),
    ok = frequency:allocate(From),
    {ok, _Freq} = get_reply(),
    ok = frequency:allocate(From),
    {error, already_allocated} = get_reply(),
    ok = frequency:stop(From),
    stopped = get_reply(),
    ok.

-spec allocate_all_frequencies_test() -> ok.
allocate_all_frequencies_test() ->
    From = self(),
    Freqs = [10, 11, 12, 13, 14, 15],
    {ok, _Server} = frequency:start(),
    Clients = [spawn(fun() -> new_client(From) end) || _ <- Freqs],
    Replys = [{frequency:allocate(Client), get_reply()} || Client <- Clients],
    Replys = [{ok, {ok, F}} || F <- Freqs],
    NewClient = spawn(fun() -> new_client(From) end),
    ok = frequency:allocate(NewClient),
    {error, no_frequency} = get_reply(),
    ok = frequency:stop(From),
    stopped = get_reply(),
    ok.

-spec deallocate_that_it_is_not_currently_using_test() -> ok.
deallocate_that_it_is_not_currently_using_test() ->
    From = self(),
    {ok, _Server} = frequency:start(),
    Replys = [{frequency:deallocate(From, N), get_reply()} || N <- lists:seq(0, 100)],
    Replys = [{ok, ok} || _ <- lists:seq(0, 100)],
    ok = frequency:stop(From),
    stopped = get_reply(),
    ok.

%% ===================================================================
%% Helper functions
%% ===================================================================

-spec get_reply() -> any().
get_reply() ->
    receive
        {reply, Reply} ->
            Reply
    end.

-spec new_client(pid()) -> any().
new_client(From) ->
    receive 
        Msg ->
            From ! Msg
    end.

        