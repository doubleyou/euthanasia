-module(euthanasia_tests).

-include_lib("eunit/include/eunit.hrl").

euthanasia_test_() ->
    {setup,
        fun() ->
            euthanasia:start()
        end,
        fun(_) ->
            application:stop(euthanasia)
        end,
        [
            {"over-messaging", fun over_messaging/0},
            {"over-memory", fun over_memory/0},
            {"over-both", fun over_both/0},
            {"garbage-collect", fun gc/0},
            {"pre-quit-hook", fun hook/0},
            {"hook-timeout", fun hook_timeout/0}
        ]
    }.

over_messaging() ->
    application:set_env(euthanasia, interval, 1000),

    Pid = spawn(fun() -> receive test -> ok end end),

    euthanasia:register(Pid),
    erlang:monitor(process, Pid),

    [Pid ! bar || _ <- lists:seq(1, 100)],

    ?assertEqual(true, is_process_alive(Pid)),

    Pid ! baz,

    receive
        {'DOWN', _, _, Pid, _} -> ok
    after 2000 ->
        throw(no_killing)
    end.

over_memory() ->
    Pid = spawn(fun() -> mem_accumulator_loop([]) end),

    euthanasia:register(Pid, [{max_memory, 10240}, {gc_attempts, {2, 100}}]),
    erlang:monitor(process, Pid),

    [Pid ! I || I <- lists:seq(1, 100)],

    ?assertEqual(true, is_process_alive(Pid)),

    [Pid ! I || I <- lists:seq(1, 100000)],

    receive
        {'DOWN', _, _, Pid, _} -> ok
    after 2000 ->
        throw(no_killing)
    end.

over_both() ->
    Pid = spawn(fun() -> receive test -> ok end end),

    euthanasia:register(Pid, [{max_memory, 10240}, {gc_attempts, {5, 1000}}]),
    erlang:monitor(process, Pid),

    [Pid ! I || I <- lists:seq(1, 10)],

    ?assertEqual(true, is_process_alive(Pid)),

    Pid ! lists:seq(1, 100000),

    [Pid ! I || I <- lists:seq(1, 100)],

    receive
        {'DOWN', _, _, Pid, _} -> ok
    after 2000 ->
        throw(no_killing)
    end.

gc() ->
    Pid = spawn(fun() -> msg_processing_loop() end),

    euthanasia:register(Pid, [{max_memory, 10240}, {gc_attempts, {5, 100}}]),
    erlang:monitor(process, Pid),

    [Pid ! I || I <- lists:seq(1, 100)],

    ?assertEqual(true, is_process_alive(Pid)),

    [Pid ! I || I <- lists:seq(1, 100000)],

    timer:sleep(3000),

    ?assertEqual(true, is_process_alive(Pid)),

    exit(Pid, kill),

    receive
        {'DOWN', _, _, Pid, _} -> ok
    after 2000 ->
        throw(no_killing)
    end.

hook() ->
    Pid = spawn(fun() -> receive test -> ok end end),

    euthanasia:register(Pid, [{pre_kill_hook, {{erlang, is_process_alive}, 1000}}]),
    erlang:monitor(process, Pid),

    [Pid ! I || I <- lists:seq(1, 200)],

    receive
        {'DOWN', _, _, Pid, _} -> ok
    after 3000 ->
        throw(no_killing)
    end.

hook_timeout() ->
    Pid = spawn(fun() -> receive test -> ok end end),

    Fun = fun(P) ->
        receive _ -> P after 2000 -> ok end
    end,

    euthanasia:register(Pid, [{pre_kill_hook, {Fun, 100}}]),
    erlang:monitor(process, Pid),

    [Pid ! I || I <- lists:seq(1, 200)],

    receive
        {'DOWN', _, _, Pid, _} -> ok
    after 3000 ->
        throw(no_killing)
    end.

mem_accumulator_loop(L) ->
    receive
        V -> mem_accumulator_loop([V | L])
    end.

msg_processing_loop() ->
    receive 
        V -> V
    end,
    msg_processing_loop().
