-module(euthanasia_sup).
-behaviour(supervisor).

-export([start_link/0]).
-export([start_child/2]).
-export([init/1]).

%%
%% External API
%%

start_child(Pid, Options) ->
    supervisor:start_child(?MODULE, [Pid, Options]).

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%%
%% supervisor callbacks
%%

init([]) ->
    Spec = {
        undefined,
        {euthanasia_monitor, start_link, []},
        transient,
        5000,
        worker,
        [euthanasia_monitor]
    },
    {ok, { {simple_one_for_one, 5, 10}, [Spec]} }.
