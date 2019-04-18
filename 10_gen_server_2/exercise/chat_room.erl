-module(chat_room).
-behavior(gen_server).

-export([start_link/0, add_user/3, remove_user/2, get_users/1, add_message/3, get_history/1]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-record(room, {users = maps:new(), history = []}).

start_link() -> gen_server:start_link(?MODULE, [],[]).

init([]) -> {ok, #room{}}.


add_user(RoomPid, UserName, UserPid) ->
    gen_server:cast(RoomPid, {add_user, {UserName, UserPid}}),
    ok.

remove_user(RoomPid, UserPid) ->
    gen_server:call(RoomPid, {remove_user, UserPid}).

get_users(RoomPid) ->
    gen_server:call(RoomPid, get_users).

add_message(RoomPid, UserName, Msg) ->
    gen_server:cast(RoomPid, {add_message, {UserName, Msg}}),
    ok.

get_history(RoomPid) ->
    gen_server:call(RoomPid, get_history).

%%% %%% HANDLE_CALL %%% %%%

handle_call({remove_user, UserPid},_From, #room{users=Users} = State) ->
    case maps:find(UserPid, Users) of
        {ok, _User} -> 
            UpdUsers = maps:remove(UserPid, Users),
            NewState = State#room{users=UpdUsers},
            {reply, ok,NewState};
        error -> {reply, {error, user_not_found}, State}
    end;

handle_call(get_users, _From, #room{users=Users} = State) ->
    Reply = maps:fold(fun(K,V,Acc) -> [{V, K}|Acc] end, [], Users),
    {reply, Reply, State};

handle_call(get_history, _From, #room{history=History} = State) ->
    {reply, lists:reverse(History), State}.

%%% HANDLE_CAST %%%

handle_cast({add_user, {UserName, UserPid}}, #room{users = Users} = State) ->
    UpdUsers = maps:put(UserPid, UserName, Users),
    NewState = State#room{users= UpdUsers},
    {noreply, NewState};

handle_cast({add_message, {UserName, Msg}}, #room{users = Users, history = History} = State) ->
    lists:foreach(fun(User) -> chat_user:add_message(User, UserName, Msg) end, maps:keys(Users)),
    NewState = State#room{history= [{UserName, Msg} | History]},
    {noreply, NewState}.

handle_info(Msg, State) ->
    io:format("Unexpected message: ~p~n", [Msg]),
    {noreply, State}.

terminate(Reason, _State) ->
    {stop, Reason}.

code_change(_PreviousVersion, State, _Extra) ->
    {ok, State}.
