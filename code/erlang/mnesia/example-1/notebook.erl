%%%----------------------------------------------------------------------------
%%% Simple example mnesia.
%%% Copyright (c) 2011, Nycholas de Oliveira e Oliveira <nycholas@gmail.com>
%%% All rights reserved.
%%%
%%% Redistribution and use in source and binary forms, with or without
%%% modification, are permitted provided that the following conditions are met:
%%%
%%% % Redistributions of source code must retain the above copyright notice,
%%%    this list of conditions and the following disclaimer.
%%% % Redistributions in binary form must reproduce the above copyright notice,
%%%    this list of conditions and the following disclaimer in the documentation
%%%    and/or other materials provided with the distribution.
%%% % Neither the name of the Nycholas de Oliveira e Oliveira nor the names of
%%%    its contributors may be used to endorse or promote products derived from
%%%    this software without specific prior written permission.
%%%
%%% THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS"
%%% AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
%%% IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
%%% ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE
%%% LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR
%%% CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF
%%% SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS
%%% INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN
%%% CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE)
%%% ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
%%% POSSIBILITY OF SUCH DAMAGE.
%%%----------------------------------------------------------------------------
-module(notebook).
-export([start/0, stop/0, create_schema/0]).
-export([list/0, read/1, insert_or_update/1, delete/1]).
-include("notebook.hrl").

start() ->
    io:format(" ++ Starting database ++~n"),
    mnesia:start().
    
stop() ->
    io:format(" ++ Stopping database ++~n"),
    mnesia:stop().

create_schema() ->
    io:format(" ++ Create schema database ++~n"),
    mnesia:create_schema([node()]),
    mnesia:start(),
    mnesia:create_table(notebook, 
			[{disc_copies,[node()]},
			 {attributes, 
			  record_info(fields, notebook)}]),
    mnesia:stop().
    
list() ->
    io:format(" ++ Notebook's list ++~n"),
    Fun = fun() ->
        case mnesia:all_keys(notebook) of
        [KeyList] ->
            io:format("List: ~p ~n", [KeyList]);
        [] ->
            io:format("Notebook not found!~n")
        end
    end,
    mnesia:transaction(Fun).
    
read(Key) ->
    io:format(" ++ Reading the notebook: ~p ++~n", [Key]),
    Fun = fun() ->
        case mnesia:read({notebook, Key}) of
        [E] ->
            io:format("Notebook: title: ~p, description: ~p, date_joined: ~p, status: ~p~n",
                [E#notebook.title, E#notebook.description, E#notebook.date_joined,
                 E#notebook.status]);
        [] ->
            io:format("Notebook ~p not found!~n", [Key])
        end
    end,
    mnesia:transaction(Fun).

insert_or_update(Notebook) ->
    {Key, Title, Description, DateJoined, Status} = Notebook,
    case mnesia:read({notebook, Key}) of
    [] ->
        io:format(" ++ Create notebook ++~n"),
        Entry = #notebook{
            title=Title, 
            description=Description, 
            date_joined=DateJoined, 
            status=Status
        },
        mnesia:write(Entry);
    [E] ->
        io:format(" ++ Update notebook: ~p ++~n", [Key]),
        Entry = E#notebook{
            title=Title, 
            description=Description, 
            date_joined=DateJoined, 
            status=Status
        },
        mnesia:write(Entry)
    end.

delete(Key) ->
    io:format(" ++ Delete notebook: ~p ++~n", [Key]),
    Fun = fun() ->
        case mnesia:read({notebook, Key}) of 
        [E] ->
            mnesia:delete_object(E);
        [] -> 
            io:format("Notebook not found!~n")
        end
    end,
    mnesia:transaction(Fun).
    
