-module(worker).
-export([parser/2, parse_strings/1]).

parser(File, Pid) ->
    {ok, Bin} = file:read_file(File),
    Strings = binary:split(Bin, <<"\n">>, [global]),
    ParsedResult = parse_strings(Strings),
    Pid ! {result, ParsedResult}.

parse_strings(Lines) ->
    lists:foldl(
    fun
        (<<>>, Acc) -> Acc;
        (Line, Acc) ->
            [_Id, Name, BinaryQuantity, _Price]= binary:split(Line, <<",">>, [global]),
            IntegerQuantity = list_to_integer(binary_to_list(BinaryQuantity)),
            Acc#{Name => IntegerQuantity}
    end,
    #{},
    Lines).