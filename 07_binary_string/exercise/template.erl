-module(template).

-export([parse/2]).

parse(Str, Data) when is_binary(Str) ->
    Split = binary:split(Str, [<<"{{">>, <<"}}">>], [global]),
    Reply = lists:map(fun(Word) ->   
                                    case maps:find(Word, Data) of
                                        error -> Word;
                                        {ok, Value} when is_integer(Value) -> integer_to_binary(Value);
                                        {ok, Value} -> Value
                                    end 
                        end, Split),
    unicode:characters_to_binary(Reply).