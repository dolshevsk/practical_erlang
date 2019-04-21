-module(worker).
-behavior(gen_server).

-export([start_link/1, ping/1]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

start_link(WorkerId) ->
    gen_server:start_link(?MODULE, [WorkerId], []).

ping(Pid) ->
    gen_server:call(Pid, ping).


init([WorkerId]) ->
    io:format("worker ~p:~p init~n", [WorkerId, self()]),
    {ok, WorkerId}.

handle_call(ping, _Form, State) ->
    {reply, {State, self()}, State};

handle_call(stop, _From, State) ->
   {stop, normal, stopped, State};

handle_call(_Request, _From, State) ->
    {reply, ok, State}.

handle_cast(_Msg, State) ->
   {noreply, State}.

handle_info(_Info, State) ->
   {noreply, State}.

terminate(Reason, State) ->
   io:format("worker ~p:~p terminate ~p~n", [State, self(), Reason]),
    ok.

code_change(_OldVsn, State, _Extra) ->
   {ok, State}.






