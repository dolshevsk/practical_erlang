-module(main).

-export([parse/1]).


-record(state, {
    statistic = #{},
    errors = #{}
}).

parse(Files) ->
    Workers = lists:foldl(
    fun(File, Acc) ->
        Pid = spawn(worker, parser, [File, self()]),
        Ref = erlang:monitor(process, Pid),
        Worker = {Pid, Ref},
        Acc#{Worker => File}
    end,
    #{},
    Files),
    loop(Workers, #state{}).


loop(Workers, #state{statistic=Stat, errors=Errors}=State) ->
    case map_size(Workers) =< 0 of
        true -> {Stat, Errors};
        false -> 
            receive
                {result, Data} -> 
                    NewState = aggr(Data, State),
                    loop(Workers, NewState);
                {'DOWN', Ref, process, Pid, Reason} ->
                    Worker = {Pid, Ref},
                    NewWorkers = maps:remove(Worker, Workers),
                    case Reason of
                        normal -> loop(NewWorkers, State);
                        _ ->
                            File = maps:get(Worker, Workers),
                            NewState = State#state{errors = maps:put(File, Reason, Errors)},
                            loop(NewWorkers, NewState)
                    end
            after
                1000 -> {error, no_reply}
            end
    end.


aggr(Data, #state{statistic=Stat} = State) ->
    NewStatistic = maps:fold(
        fun(K, V, Acc) ->
            case maps:find(K, Acc) of
                {ok, VA} -> Acc#{K := VA + V};
                error -> Acc#{K => V}
            end
        end,
    Stat,
    Data
    ),
    State#state{statistic = NewStatistic}.
