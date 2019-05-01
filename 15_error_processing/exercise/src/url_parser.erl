-module(url_parser).

-export([parse/1]).
-record(map, {protocol, domain, query = <<>>, path = [], date = undefined}).

-spec parse(binary()) -> {ok, map()} | {error, term()}.
parse(Url) -> 
    get_protocol(Url, #map{}).

get_protocol(Url, #map{} = State) ->
    case binary:split(Url, <<"://">>) of
        [Protocol, Tail] -> get_domain(Tail, State#map{protocol= Protocol});
        _ -> {error, invalid_protocol}
    end. 

get_domain(Url, #map{} = State)->
  case binary:split(Url,<<"/">>) of
      [Domain] -> get_query(<<>>, State#map{domain= Domain});
      [Domain,Tail] -> get_query(Tail, State#map{domain= Domain});
      _ -> {error, invalid_domain}
  end.

get_query(Url, #map{} = State) ->
    case Url of
        <<>> -> {ok, State};
        _ ->
            case binary:split(Url,<<"?">>) of
                [Path,Query] -> get_path(Path, State#map{query=Query});
                [Path]-> get_path(Path, State)
            end
    end.

get_path(Url, #map{} = State) ->
    Path = lists:delete(<<>>, binary:split(Url, <<"/">>, [global])),
    case Path of 
        [Y, M, D|_] -> 
                try
                    Date = {binary_to_integer(Y),binary_to_integer(M),binary_to_integer(D)},
                    {ok, State#map{path = Path, date = validate(Date)}}
                catch
                    error:badarg -> {ok, State#map{path = Path}}
                end;
        _ -> {error, State#map{path = Path}}
    end.

validate(Date) ->
    case Date  of
        {Y, M, D} when D >=1,D =< 31, M >=1, M =< 12 -> {Y,M,D};
        _ -> undefined
    end.

    
