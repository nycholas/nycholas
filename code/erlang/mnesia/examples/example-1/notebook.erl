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
-export([create/1, list/0, read/1, insert_or_update/1, delete/1]).

-include("notebook.hrl").

-revision('Revision: 0.1 ').
-created('Date: 2011/07/01 20:32:51 ').
-created_by('nycholas@gmail.com').

%%-----------------------------------------------------------------------------
%% Function: start/0
%% Purpose: Starts the application.
%% Args: No argument.
%% Returns: A ok or error (if the process is dead)
%%-----------------------------------------------------------------------------
start() ->
    io:format(" ++ Starting database ++~n"),
    mnesia:start().

%%-----------------------------------------------------------------------------
%% Function: start/0
%% Purpose: Stop the application.
%% Args: No argument.
%% Returns: A ok or error (if the process is dead)
%%-----------------------------------------------------------------------------   
stop() ->
    io:format(" ++ Stopping database ++~n"),
    mnesia:stop().

%%-----------------------------------------------------------------------------
%% Function: create_schema/1
%% Purpose: Create schema database.
%% Args: No argument.
%% Returns: A ok or error (if the process is dead)
%%-----------------------------------------------------------------------------
create_schema() ->
    io:format(" ++ Create schema database ++~n"),
    mnesia:create_schema([node()]),
    mnesia:start(),
    mnesia:create_table(notebook, 
			[{disc_copies,[node()]},
			 {attributes, 
			  record_info(fields, notebook)}]),
    mnesia:stop().
    
%%-----------------------------------------------------------------------------
%% Function: create/1
%% Purpose: Create #notebook record and return.
%% Args: Tuple {Id, Title, Description, DateJoined, Status}.
%% Returns: A #notebook record.
%%-----------------------------------------------------------------------------
create({Id, Title, Description, DateJoined, Status}) ->
    #notebook{
        id = Id, 
        title = Title, 
        description = Description,
        date_joined = DateJoined,
        status = Status}.
    
%%-----------------------------------------------------------------------------
%% Function: list/0
%% Purpose: #notebook record is list.
%% Args: No argument.
%% Returns: A list of {atomic,List} or {error, abort} (if the process is dead)
%%-----------------------------------------------------------------------------
list() ->
    io:format(" ++ Notebook's list ++~n"),
    Fun = fun() ->
        Notebook = #notebook{id = '$1', title = '$2', _ = '_'},
        mnesia:select(notebook, [{Notebook, [], ['$1', '$2']}])
    end,
    mnesia:transaction(Fun).
    
%%-----------------------------------------------------------------------------
%% Function: read/1
%% Purpose: Read information a notebook.
%% Args: E is record notebook.
%% Returns: A list of {atomic,ok} or {error, abort} (if the process is dead)
%%-----------------------------------------------------------------------------
read(Id) ->
    io:format(" ++ Reading the notebook: ~p ++~n", [Id]),
    Fun = fun() ->
        case mnesia:read({notebook, Id}) of
        [E] ->
            io:format("Notebook: id: ~p, title: ~p, description: ~p, "
                      "date_joined: ~p, status: ~p~n", 
                      [E#notebook.id, E#notebook.title, E#notebook.description, 
                       E#notebook.date_joined, E#notebook.status]);
        [] ->
            io:format("Notebook ~p not found!~n", [Id])
        end
    end,
    mnesia:transaction(Fun).

%%-----------------------------------------------------------------------------
%% Function: insert_or_update/1
%% Purpose: Insert or update a notebook.
%% Args: E is record notebook.
%% Returns: A list of {atomic,ok} or {error, abort} (if the process is dead)
%%-----------------------------------------------------------------------------
insert_or_update(E) when is_record(E, notebook) ->
    #notebook{
        id = Id, 
        title = Title, 
        description = Description, 
        date_joined = DateJoined, 
        status = Status} = E,
    Fun = fun() ->
        case mnesia:read({notebook, Id}) of
        [] ->
            io:format(" ++ Create notebook ++~n"),
            mnesia:write(E);
        [E] ->
            io:format(" ++ Update notebook: ~p ++~n", [Id]),
            Entry = E#notebook{
                id = Id,
                title = Title, 
                description = Description, 
                date_joined = DateJoined, 
                status = Status
            },
            mnesia:write(Entry)
        end
    end,
    mnesia:transaction(Fun).

%%-----------------------------------------------------------------------------
%% Function: delete/1
%% Purpose: Delete a notebook.
%% Args: Id is primary key by record notebook.
%% Returns: A list of {atomic,ok} or {error, abort} (if the process is dead)
%%-----------------------------------------------------------------------------
delete(Id) ->
    io:format(" ++ Delete notebook: ~p ++~n", [Id]),
    Fun = fun() ->
        case mnesia:read({notebook, Id}) of 
        [E] ->
            mnesia:delete_object(E);
        [] -> 
            io:format("Notebook ~p not found!~n", [Id])
        end
    end,
    mnesia:transaction(Fun).
    
