-module(mcache_server).

-export([start_link/0, start_acceptor/2, init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).


start_link() ->
    gen_server:start_link({local, server}, ?MODULE, [], []).

init([]) ->
    {ok, Port} = application:get_env(mcache, port),
    {ok, NumAcceptors} = application:get_env(mcache, numAcceptors),
    io:format("Starting server at port ~p~n", [Port]),
    {ok, ListenSocket} = gen_tcp:listen(Port, [binary, {active, false}, {reuseaddr, true}, {packet, line}]),
    [spawn(?MODULE, start_acceptor, [ID, ListenSocket]) || ID <- lists:seq(1, NumAcceptors)],
    {ok, #{}}.

start_acceptor(ID, ListenSocket) ->
    io:format("Acceptor: handle_connection ~p ~p~n", [self(), ID]),
    io:format("Waiting for client ~n"),
    {ok, AcceptSocket} = gen_tcp:accept(ListenSocket),
    io:format("Got client ~p~n", [AcceptSocket]),
    handle_connection(ID, ListenSocket, AcceptSocket).

handle_connection(ID, ListenSocket, AcceptSocket) ->
    case gen_tcp:recv(AcceptSocket, 0) of
        {ok, Msg} ->
            MsgBin = binary:part(Msg, 0, byte_size(Msg) - 2),
            Reply = call_serv(parse(MsgBin)),
            gen_tcp:send(AcceptSocket, <<Reply/binary, "\r\n">>),
            handle_connection(ID, ListenSocket, AcceptSocket);
        {error, closed} ->
            io:format("connected closed"),
            start_acceptor(ID, ListenSocket)
    end.

parse(MsgBin) ->
    Command = binary:split(MsgBin, <<" ">>),
    case Command of
        [<<"SET">>, Tail] -> {set, params(Tail)};
        [<<"GET">>, Key] -> {get, Key};
        [<<"GETS">>, Tail] -> {gets, binary:split(Tail, <<" ">>, [global])};
        [<<"DELETE">>, Key] -> {delete, Key};
        [<<"ADD">>, Tail] -> {add, params(Tail)};
        [<<"REPLACE">>, Tail] -> {replace, params(Tail)};
        [<<"APPEND">>, Tail] -> {append, params(Tail)};
        [<<"PREPEND">>, Tail] -> {prepend, params(Tail)};
        _ -> unknown
    end.

params(Tail) ->
    case binary:split(Tail, <<" ">>) of
    [Key, Value] -> {Key, Value}
    end.

call_serv(Request) ->
    gen_server:call(server, Request).


handle_call({set, {Key, Value}}, _From, State) ->
    New_State = maps:put(Key, Value, State),
    {reply, <<"STORED">>, New_State};

handle_call({get, Key}, _From, State) ->
    case maps:get(Key, State, <<"NOT FOUND">>) of
        Reply = <<"NOT FOUND">> -> Reply;
        Value -> 
            Reply = <<"VALUE ", Key/binary, " ", Value/binary, "\r\nEND">>
    end,
    {reply, Reply, State};

handle_call({gets, Keys}, _From, State) -> 
    ok;

handle_call({delete, Key}, _From, State) ->
    case maps:find(Key, State) of
        {ok, _Value} -> 
            New_State = maps:remove(Key, State),
            Reply = <<"DELETED">>,
            {reply, Reply, New_State};
        error -> 
            Reply = <<"NOT FOUND">>,
            {reply, Reply, State}
    end;

handle_call({add, Key, Value}, _From, State) ->
    case maps:find(Key, State) of
        {ok, Value} -> 
            Reply = <<"EXISTS">>,
            {reply, Reply, State};
        error -> 
            New_State = maps:put(Key, Value),
            Reply = <<"STORED">>,
            {reply, Reply, New_State}
    end;

handle_call({replace, Key, Value}, _From, State) ->
    case maps:find(Key, State) of
        {ok, _Value} -> 
            New_State = maps:update(Key, Value, State),
            Reply = <<"STORED">>,
            {reply, Reply, New_State};
        error -> 
            Reply = <<"NOT FOUND">>,
            {reply, Reply, State}
    end;

handle_call({append, Key, Value}, _From, State) ->
    case maps:find(Key, State) of
        {ok, OldValue} -> 
            NewValue = <<OldValue/binary, Value/binary>>,
            New_State = maps:update(Key, NewValue, State),
            Reply = <<"STORED">>,
            {reply, Reply, New_State};
        error -> 
            Reply = <<"NOT FOUND">>,
            {reply, Reply, State}
    end;

handle_call({append, Key, Value}, _From, State) ->
    case maps:find(Key, State) of
        {ok, OldValue} -> 
            NewValue = <<Value/binary, OldValue/binary>>,
            New_State = maps:update(Key, NewValue, State),
            Reply = <<"STORED">>,
            {reply, Reply, New_State};
        error -> 
            Reply = <<"NOT FOUND">>,
            {reply, Reply, State}
    end;

handle_call(unknown, _From, State) ->
    {reply, <<"UNKNOWN REQUEST">>, State}.

handle_cast(_Any, State) ->
    {noreply, State}.

handle_info(Msg, State) ->
    io:format("Unexpected message: ~p~n", [Msg]),
    {noreply, State}.

terminate(Reason, _State) ->
    {stop, Reason}.

code_change(_PreviousVersion, State, _Extra) ->
    {ok, State}.