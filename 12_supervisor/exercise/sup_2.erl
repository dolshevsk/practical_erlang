-module(sup_2).

-export([start_link/0, init/1, add_worker/1, remove_worker/1]).

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).


init(_Args) ->

    SupervisorSpecification = #{
        strategy => one_for_one,
        intensity => 10,
        period => 60},
    {ok, {SupervisorSpecification, [spec(worker_3), spec(worker_4)]}}.


spec(WorkerId) ->
    #{id => WorkerId,
      start => {worker, start_link, [WorkerId]},
      restart => permanent,
      shutdown => 2000,
      type => worker,
      modules => [worker]
     }.

add_worker(WorkerId) ->
    supervisor:start_child(?MODULE, spec(WorkerId)).


remove_worker(WorkerId) ->
    supervisor:terminate_child(?MODULE, WorkerId),
    supervisor:delete_child(?MODULE, WorkerId).