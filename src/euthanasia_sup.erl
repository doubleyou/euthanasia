-module(euthanasia_sup).
-behaviour(supervisor).

-export([start_link/0]).
-export([start_child/2, stop_child/1]).
-export([init/1]).

%%
%% External API
%%

start_child(Pid, Options) ->
    Spec = {
        Pid,
        {euthanasia_monitor, start_link, [Pid, Options]},
        transient,
        5000,
        worker,
        [euthanasia_monitor]
    },
    supervisor:start_child(?MODULE, Spec).

stop_child(Pid) ->
    supervisor:terminate_child(?MODULE, Pid),
    supervisor:delete_child(?MODULE, Pid),
    ok.

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%%
%% supervisor callbacks
%%

init([]) ->
    {ok, { {one_for_one, 5, 10}, []} }.
