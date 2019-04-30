-module(worker).
-export([parser/2, parse_lines/1]).

parser(File, Pid) ->
    {ok, Bin} = file:read_file(File),
    Lines = binary:split(Bin, <<"\n">>, [global]),
    ParsedResult = parse_lines(Lines),
    Pid ! {result, ParsedResult}.

parse_lines(Lines) ->
    lists:foldl(
        fun
            (<<>>, Acc) -> Acc;
            (Line, Acc) ->
                [_Id, Name, Quantity, _Price]
                    = binary:split(Line, <<",">>, [global]),
                Quantity2 = list_to_integer(binary_to_list(Quantity)),
                Acc#{Name => Quantity2}
        end,
        #{},
        Lines).