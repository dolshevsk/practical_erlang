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