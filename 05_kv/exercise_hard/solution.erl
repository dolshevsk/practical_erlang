-module(solution).

-export([init_users/0, init_sessions/0, group_by/1, group_by/2, by_age/1, by_age_range/1, by_node/1, by_type/1]).

init_users() ->
    [
        {user, "Bob", 21, male},
        {user, "Bill", 23, male},
        {user, "Helen", 17, female},
        {user, "Kate", 25, female},
        {user, "John", 20, male}
    ].


group_by(Users) ->
    lists:foldl(fun({user, _Name, _Age, Sex} = User, Dict) ->
                        case Sex of 
                            male -> maps:update(male, [User|maps:get(male, Dict)], Dict);
                            female -> maps:update(female, [User|maps:get(female, Dict)], Dict)
                        end 
                end, #{male=>[], female => []}, Users).


init_sessions() ->
    [
        {session, type_a, node_1, 1},
        {session, type_b, node_1, 2},
        {session, type_a, node_2, 3},
        {session, type_b, node_2, 4}
    ].

group_by(CriteriaFun, List) ->
    lists:foldl(
        fun
            (Item, Groups) ->
                Group = CriteriaFun(Item),

                Groups#{
                    Group =>
                    [Item | maps:get(Group, Groups, [])]
                }
        end,
        #{},
        List
    ).

by_age({user, _Name, Age, _Gender}) -> Age.

by_age_range({user, _Name, Age, _Gender}) ->
	if
		Age > 0 andalso Age =< 12 -> child;
		Age > 12 andalso Age =< 18 -> teenage;
		Age > 18 andalso Age =< 25 -> young;
		Age > 25 andalso Age =< 60 -> adult;
		true -> old
	end.

by_node({session, _Type, Node, _SocketId}) -> Node.

by_type({session, Type, _Node, _SocketId}) -> Type.