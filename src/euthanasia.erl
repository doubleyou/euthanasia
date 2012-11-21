-module(euthanasia).

-compile({no_auto_import,[register/2]}).

-export([start/0]).
-export([register/1, register/2]).

start() ->
    application:start(euthanasia).

register(Pid) ->
    register(Pid, []).

register(Pid, Options) ->
    {ok, _} = euthanasia_sup:start_child(Pid, Options),
    ok.
