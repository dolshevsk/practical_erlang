-module(chat_user).
-behavior(gen_server).

-export([start_link/0, add_message/3, get_messages/1]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

start_link() -> gen_server:start_link(?MODULE, [],[]).

init([]) -> {ok, []}.

add_message(Pid, Username, Msg) ->
    gen_server:cast(Pid, {add_message, {Username, Msg}}),
    ok.

get_messages(Pid) ->
    gen_server:call(Pid, get_messages).

handle_cast({add_message, Msg}, State) ->
    NewState = [Msg|State],
    {noreply, NewState}.

handle_call(get_messages, _From, State) ->
    {reply, lists:reverse(State), State}.

handle_info(Msg, State) ->
    io:format("Unexpected message: ~p~n", [Msg]),
    {noreply, State}.

terminate(Reason, _State) ->
    {stop, Reason}.

code_change(_PreviousVersion, State, _Extra) ->
    {ok, State}.