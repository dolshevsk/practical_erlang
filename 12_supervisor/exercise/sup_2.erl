-module(sup_2).

-export([start_link/0, init/1, add_worker/1, remove_worker/1]).

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).


init(_Args) ->

    SupervisorSpecification = #{
        strategy => one_for_one,
        intensity => 10,
        period => 60},

    ChildSpecifications = [childSpecifications(worker_3), childSpecifications(worker_4) ],

    {ok, {SupervisorSpecification, ChildSpecifications}}.

childSpecifications(Worker) ->
    #{
            id => Worker,
            start => {worker, start_link, [4]},
            restart => permanent, 
            shutdown => 2000,
            type => worker, 
            modules => [worker]
        }.


add_worker(Id) ->
    supervisor:start_child(
        ?MODULE, childSpecifications(Id)
    ).


remove_worker(WorkerId) ->
    supervisor:terminate_child(?MODULE, WorkerId),
    supervisor:delete_child(?MODULE, WorkerId).