% Simple example server - application distributed.
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
-module(etalk_languages).

-export([start_link/0, add_handler/3, delete_handler/3,
         which_handlers/1, notify/2]).

start_link() -> 
    {ok, Handler} = gen_event:start_link(?MODULE),
    gen_event:add_handler(Handler, portuguese_language, []),
    gen_event:add_handler(Handler, english_language, []),
    gen_event:add_handler(Handler, italian_language, []),
    {ok, Handler}.

add_handler(EventMgrRef, Handler, Args)->
    case gen_event:add_handler(EventMgrRef, Handler, Args) of
        ok ->
            ok;
        {'EXIT', Reason} ->
            error_logger:error_msg("Problem loading plugin ~p ~p ~n", [Handler, Reason]);
        Other ->
            error_logger:error_msg("Loading ~p reports ~p ~n", [Handler, Other])
    end.

delete_handler(EventMgrRef, Handler, Args)->
    case gen_event:delete_handler(EventMgrRef, Handler, Args) of
        ok ->
            ok;
        {'EXIT', Reason} ->
            error_logger:error_msg("Problem deleting plugin ~p ~p ~n", [Handler, Reason]);
        Other ->
            error_logger:error_msg("Deleting ~p reports ~p ~n", [Handler, Other])
    end.

which_handlers(EventMgrRef) ->
    gen_event:which_handlers(EventMgrRef).

notify(EventMgrRef, Msg) ->
    gen_event:notify(EventMgrRef, Msg).
