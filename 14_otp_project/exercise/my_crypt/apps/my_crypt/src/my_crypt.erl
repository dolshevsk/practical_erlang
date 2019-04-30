-module(my_crypt).

-behaviour(gen_server).

%% API
-export([start_link/0, encode/1, get_key/0, set_key/1, hash/1]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).


start_link() ->
   gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

init([]) ->
  {ok, Key} = application:get_env(my_crypt, encode_key),
  State = #{key => Key, hashtable => Hashtable},
  {ok,State}.

encode(Data)->
  gen_server:call(?MODULE,{encode, Data}).

get_key()->
  gen_server:call(?MODULE,get_key).

set_key(NewKey)->
  gen_server:cast(?MODULE,{set_key, NewKey}).


handle_call(stop, _From, State) ->
   {stop, normal, stopped, State};

handle_call(_Request, _From, State) ->
    {reply, ok, State}.

handle_cast(_Msg, State) ->
   {noreply, State}.

handle_info(_Info, State) ->
   {noreply, State}.

terminate(_Reason, _State) ->
   ok.

code_change(_OldVsn, State, _Extra) ->
   {ok, State}.





