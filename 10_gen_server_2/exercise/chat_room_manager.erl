-module(chat_room_manager).
-behavior(gen_server).

-export([start_link/0,
         create_room/1, get_rooms/0,
         add_user/3, remove_user/2, get_users/1,
         send_message/3,  get_history/1]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-record(state, {rooms = maps:new()}).

start_link() -> gen_server:start_link({local, manager}, ?MODULE, [], []).

init([]) -> {ok, #state{}}.


create_room(RoomName) ->
    {ok, RoomPid} = chat_room:start_link(),
    Room = {RoomName, RoomPid},
    gen_server:cast(manager, {create_room, Room}),
    Room.

get_rooms() ->
    gen_server:call(manager, get_rooms).

add_user(RoomPid, UserName, UserPid) ->
    gen_server:call(manager, {add_user, {RoomPid, UserName, UserPid}}).

remove_user(RoomPid, UserPid) ->
    gen_server:call(manager, {remove_user, RoomPid, UserPid}).

get_users(RoomPid) ->
    gen_server:call(manager, {get_users, RoomPid}).

send_message(RoomPid, UserName, Msg) ->
    gen_server:call(manager, {send_message, {RoomPid, UserName, Msg}}).

get_history(RoomPid) ->
    gen_server:call(manager, {get_history, RoomPid}).

%%% HANDLE CALL %%%

handle_call(get_rooms, _From, #state{rooms = Rooms} = State) ->
    Reply = maps:fold(fun(K,V,Acc) -> [{V, K}|Acc] end, [], Rooms),
    {reply, Reply, State};

handle_call({add_user, {RoomPid, UserName, UserPid}}, _From, #state{rooms = Rooms} = State) ->
           case maps:find(RoomPid, Rooms) of
                {ok, _RoomName} -> {reply, chat_room:add_user(RoomPid, UserName, UserPid), State};
                error -> {reply, {error, room_not_found}, State}
            end;

handle_call({remove_user, RoomPid, UserPid}, _From, #state{rooms = Rooms} = State) ->
             case maps:find(RoomPid, Rooms) of
                {ok, _RoomName} -> {reply, chat_room:remove_user(RoomPid, UserPid), State};
                error -> {reply, {error, room_not_found}, State}
            end;

handle_call({get_users, RoomPid}, _From, #state{rooms = Rooms} = State) ->
            case maps:find(RoomPid, Rooms) of
                {ok, _RoomName} -> {reply, {ok, chat_room:get_users(RoomPid)}, State};
                error -> {reply, {error, room_not_found}, State}
            end;

handle_call({send_message, {RoomPid, UserName, Msg}}, _From, #state{rooms = Rooms} = State) ->
            case maps:find(RoomPid, Rooms) of
                {ok, _RoomName} -> {reply, chat_room:add_message(RoomPid, UserName, Msg), State};
                error -> {reply, {error, room_not_found}, State}
            end;

handle_call({get_history, RoomPid}, _From, #state{rooms = Rooms} = State) ->
            case maps:find(RoomPid, Rooms) of
                {ok, _RoomName} -> {reply, {ok, chat_room:get_history(RoomPid)}, State};
                error -> {reply, {error, room_not_found}, State}
            end.

%%% HANDLE CAST %%%

handle_cast({create_room, {RoomName, RoomPid}}, #state{rooms = Rooms} = State) ->
    UpdRooms = maps:put(RoomPid, RoomName, Rooms),
    NewState = State#state{rooms = UpdRooms},
    {noreply, NewState}.

handle_info(Msg, State) ->
    io:format("Unexpected message: ~p~n", [Msg]),
    {noreply, State}.

terminate(Reason, _State) ->
    {stop, Reason}.

code_change(_PreviousVersion, State, _Extra) ->
    {ok, State}.
