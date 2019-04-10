-module(map_reduce).

-export([start/0, start/1, reducer/3, worker/3]).

start() ->
    start(["data1.txt", "data2.txt", "data3.txt", "data4.txt", "data5.txt"]).

start(Files) ->
    io:format("start ~p~n", [self()]),
    Ref = make_ref(),
    spawn(?MODULE, reducer, [self(), Ref, Files]),
    receive 
        {Ref, Res} -> {ok, Res}
    after
        3000 -> {error, no_reply}
    end.

reducer(FromPid, Ref, Files) ->
    io:format("reduce ~p~n", [self()]),
    Wait = lists:map(
        fun(File) ->
            WRef = make_ref(),
            WPid = spawn(?MODULE, worker, [self(), WRef, File]),
            {WRef, WPid}
        end, Files),
    Res = loop(Wait, #{}),
    FromPid ! {Ref, Res}.

loop([], Acc)-> Acc;
loop([{Ref, _}|Wait], Acc) ->
    receive
        {Ref, Res} -> loop(Wait, aggregate(Res, Acc))
    after
        5000 ->
            io:format("ERROR: no reply from worker, ~p~p~n", [Ref,Wait])
    end.

worker(ReducerPid, Ref, File) ->
    io:format("worker ~p~n", [self()]),
    Res = parse_file(File),
    ReducerPid ! {Ref, Res}.

parse_file(File)->
    {ok, Content} = file:read_file(File),
    Words = binary:split(Content, [<<" ">>, <<"\n">>, <<"\r">>], [global]),
    Words2 = lists:filter(fun(Word) -> Word /= <<>> end, Words),
    Words3 = lists:map(fun unicode:characters_to_list/1, Words2),
    lists:foldl(fun(Word, Acc)-> Acc#{Word => maps:get(Word, Acc, 0)+1} end, #{}, Words3).

aggregate(Map1, Map2) ->
    maps:fold(fun(K,V,Acc)-> 
        case maps:find(K, Acc) of
            {ok, VA} -> Acc#{K := VA + V};
            error -> Acc#{K => V}
            end
            
        end, Map1, Map2).