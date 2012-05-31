-module(esocketcho_serv).
-behaviour(gen_server).

-record(state, {name, 
                next,
                socket}).

-export([start_link/1]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         code_change/3, terminate/2]).

%% Publics
start_link(Socket) ->
    gen_server:start_link(?MODULE, Socket, []).

%% Behaviours
init(Socket) ->
    gen_server:cast(self(), first),
    {ok, #state{socket=Socket}}.

handle_call(_Msg, _From, State) ->
    {noreply, State}.

handle_cast(first, S = #state{socket=ListenSocket}) ->
    {ok, AcceptSocket} = gen_tcp:accept(ListenSocket),
    esocketcho_sup:start_socket(),
    send(AcceptSocket, "What's your name?", []),
    {noreply, S#state{socket=AcceptSocket, next=name}};

handle_cast(welcome, S = #state{socket=Socket}) ->
    send(Socket, "Pleasure, my name is E, ~p let's talk?", [S#state.name]),
    {noreply, S#state{next=choose_talk}};

handle_cast(talk_accepted, S = #state{socket=Socket}) ->
    send(Socket, "I remembered that I have something else more "
                 "important to do, bye!", []),
    gen_server:cast(self(), quit),
    {noreply, S};

handle_cast(finish, S = #state{socket=Socket}) ->
    send(Socket, "So long and good luck!", []),
    gen_server:cast(self(), quit),
    {noreply, S};

handle_cast(quit, S) ->
    gen_tcp:close(S#state.socket),
    {stop, normal, S};

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info({tcp, _Socket, Str}, S = #state{next=name}) ->
    Name = line(Str),
    gen_server:cast(self(), welcome),
    {noreply, S#state{name=Name, next=welcome}};

handle_info({tcp, Socket, Str}, S = #state{socket=Socket, next=choose_talk}) ->
    case line(Str) of
        "y" ->
            gen_server:cast(self(), talk_accepted);
        "n" -> 
            gen_server:cast(self(), finish);
        _ ->
            send(Socket, "Answer with y (yes) or n (no)", [])
    end,
    {noreply, S};

handle_info({tcp, _Socket, "quit" ++ _}, S) ->
    gen_tcp:close(S#state.socket),
    {stop, normal, S};

handle_info({tcp_closed, _}, S) ->
    {stop, normal, S};

handle_info(E, S) ->
    io:format("unexpected: ~p~n", [E]),
    {noreply, S}.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

terminate(normal, _State) ->
    ok;

terminate(Reason, _State) ->
    io:format("terminate reason: ~p~n", [Reason]).

%% Privates
send(Socket, Msg, Args) ->
    gen_tcp:send(Socket, io_lib:format(Msg ++ "~n", Args)),
    inet:setopts(Socket, [{active, once}]),
    ok.

line(Str) ->
    List = string:tokens(Str, "\r\n "),
    if 
        List == [] ->
            "";
        true ->
            hd(List)
    end.

