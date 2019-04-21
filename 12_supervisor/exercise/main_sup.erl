-module(main_sup).

-export([start_link/0, init/1]).

start_link() ->
    supervisor:start_link(?MODULE, []).


init(_Args) ->
    SupervisorSpecification = #{
        strategy => one_for_one,
        intensity => 10,
        period => 60},
    
    Super1 =  #{id => super_1,
                   start => {sup_1, start_link, []},
                   restart => permanent,
                   shutdown => 2000,
                   type => supervisor,
                   modules => [sup_1]
                  },
   
    Super2 =  #{id => child_super_2,
                   start => {sup_2, start_link, []},
                   restart => permanent,
                   shutdown => 2000,
                   type => supervisor,
                   modules => [sup_2]
                  },
    
    {ok, {SupervisorSpecification, [Super1, Super2]}}.
