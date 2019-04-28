-module(mcache_server).

-export([start_link/0, start_acceptor/2, init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).


start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

init([]) ->
    {ok, Port} = application:get_env(mcache, port),
    {ok, NumAcceptors} = application:get_env(mcache, numAcceptors),
    io:format("Starting server at port ~p~n", [Port]),
    {ok, ListenSocket} = gen_tcp:listen(Port, [binary, {active, false}, {reuseaddr, true}, {packet, line}]),
    [spawn(?MODULE, start_acceptor, [ID, ListenSocket]) || ID <- lists:seq(1, NumAcceptors)],
    {ok, ListenSocket}.

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
            Reply = handle(parse_protocol(MsgBin)),
            BinId = list_to_binary(integer_to_list(ID)),
            gen_tcp:send(AcceptSocket, <<"From", BinId/binary, ":" ,Reply/binary, "\r\n">>),
            handle_connection(ID, ListenSocket, AcceptSocket);
        {error, closed} ->
            io:format("connected closed"),
            start_acceptor(ID, ListenSocket)
    end.


parse_msg(Data) ->
    Parts = binary:split(Data, <<" ">>),
    case Parts of
        [<<"SET">>, Rest] -> key_value(set, Rest);
        [<<"GET">>, Key] -> {get, Key};
        [<<"GETS">>, Keys] -> {gets, binary:split(Keys, <<" ">>, [global])};
        [<<"ADD">>, Rest] -> key_value(add, Rest);
        [<<"REPLACE">>, Rest] -> key_value(replace, Rest);
        [<<"APPEND">>, Rest] -> key_value(append, Rest);
        [<<"PREPEND">>, Rest] -> key_value(prepend, Rest);
        [<<"DELETE">>, Key] -> {delete, Key};
        _ -> <<"UNEXPECTED MESSAGE">>
    end.

key_value(Action, Data) ->
    case binary:split(Data, <<" ">>) of
        [Key, Value] -> {Action, Key, Value};
        _ -> <<"UNEXPECTED MESSAGE">>
    end.

handle({set, Key, Value}) ->
    mcache_storage:set(Key, Value),
    <<"STORED">>;

handle({get, Key}) ->
    case mcache_storage:get(Key) of
        {ok, Value} -> <<"VALUE ", Key/binary, " ", Value/binary, "\r\nEND">>;
        {error, not_found} -> <<"NOT FOUND">>
    end;

handle({gets, Keys}) ->
    {[], Res} = lists:foldl(
                  fun(R, {[Key | Rest], Acc}) ->
                          Value = case R of
                                      {ok, V} -> V;
                                      {error, not_found} -> <<"NOT FOUND">>
                                  end,
                          {Rest, <<Acc/binary, "VALUE ", Key/binary, " ", Value/binary, "\r\n">>}
                  end,
                  {Keys, <<>>},
                  mcache_storage:gets(Keys)),
    <<Res/binary, "END">>;

handle({delete, Key}) ->
    case mcache:delete(Key) of
        ok -> <<"DELETED">>;
        {error, not_found} -> <<"NOT FOUND">>
    end.

handle({add, Key, Value}) ->
    case mcache:add(Key, Value) of
        ok -> <<"STORED">>;
        {error, exists} -> <<"EXISTS">>
    end;

handle({replace, Key, Value}) ->
    case mcache:replace(Key, Value) of
        ok -> <<"STORED">>;
        {error, not_found} -> <<"NOT FOUND">>
    end;

handle({append, Key, Value}) ->
    case mcache:append(Key, Value) of
        ok -> <<"STORED">>;
        {error, not_found} -> <<"NOT FOUND">>
    end;

handle({prepend, Key, Value}) ->
    case mcache:prepend(Key, Value) of
        ok -> <<"STORED">>;
        {error, not_found} -> <<"NOT FOUND">>
    end;


handle_call(_Any, _From, State) ->
    {noreply, State}.

handle_cast(_Any, State) ->
    {noreply, State}.

handle_info(Msg, State) ->
    io:format("Unexpected message: ~p~n", [Msg]),
    {noreply, State}.

terminate(Reason, _State) ->
    {stop, Reason}.

code_change(_PreviousVersion, State, _Extra) ->
    {ok, State}.