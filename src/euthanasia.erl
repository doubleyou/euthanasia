-module(euthanasia).

-compile({no_auto_import,[register/2, unregister/1]}).

-export([start/0]).
-export([register/0, register/1, register/2, unregister/1]).

start() ->
    application:start(euthanasia).

register() ->
    register(self()).

register(Pid) ->
    register(Pid, []).

register(Pid, Options) ->
    {ok, _} = euthanasia_sup:start_child(Pid, Options),
    ok.

unregister(Pid) ->
    euthanasia_sup:stop_child(Pid).
