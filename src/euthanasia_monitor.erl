-module(euthanasia_monitor).
-behaviour(gen_server).

-export([start_link/2]).
-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         code_change/3,
         terminate/2]).

-record(state, {
    pid,
    ref,
    gc_ref,
    max_queue_len,
    interval,
    max_memory,
    gc_attempts,
    gc_attempts_left,
    pre_kill_hook
}).

%%
%% External API
%%

start_link(Pid, Options) ->
    gen_server:start_link(?MODULE, [Pid, Options], []).

%%
%% gen_server callbacks
%%

init([Pid, Options]) ->
    Interval = get_opt(interval, Options),
    {GCAttempts, GCTimeout} = get_opt(gc_attempts, Options),
    State = #state{
        pid = Pid,
        ref = erlang:monitor(process, Pid),
        max_queue_len = get_opt(max_queue_len, Options),
        interval = Interval,
        max_memory = get_opt(max_memory, Options),
        gc_attempts = {GCAttempts, GCTimeout},
        gc_attempts_left = GCAttempts,
        pre_kill_hook = get_opt(pre_kill_hook, Options)
    },
    schedule_check(Interval),
    {ok, State}.

handle_call(Msg, _From, State) ->
    {reply, {error, {unknown_call, Msg}}, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(poll, State = #state{interval=Interval}) ->
    Result = check_process_status(State),
    schedule_check(Interval),
    Result;
handle_info({timeout, GCRef, gc}, State = #state{gc_ref=GCRef}) ->
    check_process_memory(State);
handle_info({'DOWN', Ref, process, Pid, Reason}, S=#state{ref=Ref, pid=Pid}) ->
    error_logger:error_report([
        "Euthanasia: monitored process unexpectedly exited",
        {pid, Pid},
        {reason, Reason}
    ]),
    {stop, normal, S};
handle_info(_Info, State) ->
    {noreply, State}.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

terminate(_Reason, _State) ->
    ok.

%%
%% Internal functions
%%

check_process_status(State=#state{pid=Pid, max_queue_len=MQL, gc_ref=GCRef}) ->
    case process_info(Pid, message_queue_len) of
        {message_queue_len, Length} when Length > MQL ->
            mercy_kill(State);
        {message_queue_len, _} ->
            case GCRef of
                undefined ->
                    check_process_memory(State);
                _ ->
                    %% Non-undefined gc_ref means that we are currently
                    %% trying to garbage collect the process
                    {noreply, State}
            end;
        undefined ->
            %% This means process is already dead and we are going to receive
            %% the {'DOWN', _, _, _, _} tuple
            {noreply, State}
    end.

check_process_memory(State = #state{pid=Pid, max_memory=MM, gc_ref=GCRef,
                                        gc_attempts={GCAttempts, GCInterval},
                                        gc_attempts_left = GCAttemptsLeft}) ->
    case process_info(Pid, memory) of
        {memory, Mem} when Mem > MM andalso GCAttemptsLeft =:= 0 ->
            mercy_kill(State);
        {memory, Mem} when Mem > MM andalso GCAttemptsLeft =:= 1 ->
            erlang:garbage_collect(Pid),
            check_process_memory(State#state{gc_attempts_left=0});
        {memory, Mem} when Mem > MM ->
            erlang:garbage_collect(Pid),
            {noreply, State#state{
                gc_attempts_left=GCAttemptsLeft-1,
                gc_ref=erlang:start_timer(GCInterval, self(), gc)
            }};
        {memory, _} ->
            GCRef =:= undefined orelse erlang:cancel_timer(GCRef),
            {noreply, State#state{
                gc_attempts_left=GCAttempts,
                gc_ref=undefined
            }};
        undefined ->
            %% This means process is already dead and we are going to receive
            %% the {'DOWN', _, _, _, _} tuple
            {noreply, State}
    end.

mercy_kill(State = #state{pid=Pid, ref=Ref, pre_kill_hook=undefined}) ->
    erlang:demonitor(Ref),
    exit(Pid, kill),
    {stop, normal, State};
mercy_kill(State = #state{pid=Pid, pre_kill_hook={Hook, Timeout}}) ->
    Self = self(),
    HookRef = make_ref(),
    HookPid = spawn(fun() -> Self ! {HookRef, exec_hook(Hook, Pid)} end),
    receive
        {HookRef, _} -> ok
    after Timeout ->
        exit(HookPid, kill)
    end,
    mercy_kill(State#state{pre_kill_hook=undefined}).

exec_hook({M, F}, Pid) ->
    M:F(Pid);
exec_hook(F, Pid) ->
    F(Pid).

get_opt(Key, Options) ->
    proplists:get_value(Key, Options, get_env(Key)).

get_env(Key) ->
    case application:get_env(euthanasia, Key) of
        {ok, V} ->
            V;
        undefined ->
            proplists:get_value(Key, defaults())
    end.

defaults() ->
    [
        {max_queue_len, 100},
        {interval, 1000},
        {max_memory, 20971520},
        {gc_attempts, {2, 1000}},
        {pre_kill_hook, undefined}
    ].

schedule_check(Interval) ->
    erlang:send_after(Interval, self(), poll).
