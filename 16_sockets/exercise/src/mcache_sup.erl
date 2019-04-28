-module(mcache_sup).
-behaviour(supervisor).

-export([start_link/0, init/1]).


start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).


init(_Args) ->
    SupervisorSpecification =
        #{strategy => one_for_one, 
          intensity => 10, 
          period => 60},

    ChildSpecifications =
        [
         #{id => server,
           start => {mcache_server, start_link, []},
           restart => permanent,
           shutdown => 2000,
           type => worker,
           modules => [mcache_server]
          }
        ],
    {ok, {SupervisorSpecification, ChildSpecifications}}.
