-module(mcache_app).
-behaviour(application).

-export([start/0, start/2, stop/1]).

start() ->
    application:start(mcache),
    ok.

start(_How, _Args) ->
    mcache_sup:start_link().

stop(_State) ->
    ok.