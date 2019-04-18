-module(chat_room_manager).

-export([start/0, loop/1,
         create_room/2, remove_room/2, get_rooms/1,
         add_user/3, remove_user/3, get_users_list/2,
         send_message/4,  get_messages_history/2]).

-define(MAX_ROOMS, 5).

-record(room, {id ="", name = "", users = [], history = []}).

-record(state, {rooms = maps:new()}).

start() ->
    spawn(?MODULE, loop, [#state{}]).


create_room(Server, RoomName) ->
    call(Server, {create, RoomName}).


remove_room(Server, RoomId) ->
    call(Server, {remove, RoomId}).


get_rooms(Server) ->
    call(Server, get_rooms).


add_user(Server, RoomId, UserName) ->
    call(Server, {add_user, RoomId, UserName}).


remove_user(Server, RoomId, UserName) ->
    call(Server, {remove_user, RoomId, UserName}).


get_users_list(Server, RoomId) ->
    call(Server, {get_users, RoomId}).


send_message(Server, RoomId, UserName, Message) ->
    call(Server, {send_message, RoomId, UserName, Message}).


get_messages_history(Server, RoomId) ->
    call(Server, {get_messages_history, RoomId}).

call(Pid, Mes) ->
    Ref = erlang:monitor(process, Pid),
    Pid ! {call, Ref, self(), Mes},
    receive 
        {reply, Ref, Reply} -> 
            erlang:demonitor(Ref),
            Reply;
        {'DOWN',Ref,process,Pid, Reason} ->
            {error, Reason}
    after 500 -> 
        erlang:demonitor(Ref, [flush]),
        timeout
    end.

loop(State) ->
    io:format("loop begin~p~n",[self()]),
    receive 
        {call, Ref, From, Msg} -> {Reply, Newstate} = handle_call(Msg, State),
                From ! {reply, Ref, Reply},
                ?MODULE:loop(Newstate);
        stop -> io:format("me stop ~p~n",[self()]);
        Unknown -> io:format("Unknown message, sorry - ~p~n", [Unknown]),
            ?MODULE:loop(State)
    end.

handle_call({create, RoomName}, #state{rooms = Rooms} = State) -> 
    case maps:size(Rooms) of
        ?MAX_ROOMS -> {{error, room_limit}, State};
    _Else ->
        NewRoom = #room{id = make_ref(), name=RoomName},
        NewState = State#state{rooms = maps:put(NewRoom#room.id, NewRoom, Rooms)},
        {{ok, NewRoom#room.id}, NewState}
    end;

handle_call({remove, RoomId}, #state{rooms = Rooms} = State) ->
    case maps:find(RoomId, Rooms) of
        {ok, _} -> 
            NewState = State#state{rooms=maps:remove(RoomId, Rooms)},
            {ok, NewState};
        error -> {{error, room_not_found}, State}
    end;

handle_call({add_user, RoomId, UserName}, #state{rooms = Rooms} = State) ->
    case maps:find(RoomId, Rooms) of
        {ok, Room} -> #room{users = Users} = Room,
                      case lists:member(UserName, Users) of
                           true -> {{error, user_is_in_room}, State};
                           false -> UpdRoom = Room#room{users = [UserName | Users]},
                                    NewState = State#state{rooms = maps:update(RoomId, UpdRoom, Rooms)},
                                    {ok, NewState}
                      end;
        error -> {{error, room_not_found}, State}
    end;

handle_call({remove_user, RoomId, UserName}, #state{rooms = Rooms} = State) ->
    case maps:find(RoomId, Rooms) of
        {ok, #room {users=Users} = Room} -> 
            case lists:member(UserName, Users) of
                true -> UpdRoom = Room#room {users = lists:delete(UserName, Users)},
                        NewState = State#state {rooms = maps:update(RoomId, UpdRoom, Rooms)},
                        {ok, NewState};
                false -> {{error, user_not_in_room}, State}
            end;
        error -> {{error, room_not_found}, State}
    end;

handle_call({get_users, RoomId}, #state {rooms=Rooms} = State) ->
    case maps:find(RoomId, Rooms) of
        {ok, #room {users=Users}} -> {{ok, Users} ,State};
        error -> {{error, room_not_found}, State}
    end;

handle_call({send_message, RoomId, UserName, Message}, #state {rooms=Rooms}= State) ->
    case maps:find(RoomId, Rooms) of
        {ok, #room {users=Users, history = History} = Room} -> 
            case lists:member(UserName, Users) of
                true -> UpdRoom = Room#room{history=[{UserName, Message}|History]},
                        NewState = State#state {rooms = maps:update(RoomId, UpdRoom, Rooms)},
                        {ok, NewState};
                false -> {{error, user_not_in_room}, State}
            end;
        error -> {{error, room_not_found}, State}
    end;

handle_call({get_messages_history, RoomId}, #state{rooms=Rooms} = State) ->
    case maps:find(RoomId, Rooms) of
        {ok, #room {history = History}} -> {{ok, History}, State};
        error -> {{error, room_not_found}, State}
    end;

handle_call(get_rooms, #state{rooms=Rooms} = State) ->
    Values = maps:values(Rooms),
    All = lists:foldr(fun(Room, Acc) -> [{Room#room.id, Room#room.name}|Acc] end, [], Values),
    {All,State}.

