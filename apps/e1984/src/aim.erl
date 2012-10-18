-module(aim).

-behaviour(gen_server).

-export([start_link/4]).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-define(SERVER, ?MODULE).

-record(state, {cb_module,
                metrics,
                aim_name,
                timer,
                timer_int}).

start_link(Name, Cb, Metrics, TimeInterval) ->
    gen_server:start_link(?MODULE,
                          [Name, Cb, Metrics, TimeInterval],
                          []).

init([Name, Cb, Metrics, TimeInterval]) ->
    gen_server:cast(self(), {start, TimeInterval}),
    {ok, #state{cb_module=Cb, metrics=Metrics,
                aim_name=Name, timer_int=TimeInterval}}.

handle_call(_Request, _From, State) ->
    {reply, ok, State}.

handle_cast({start, TimeInt}, State=#state{cb_module=Cb}) ->
    ok = Cb:pre_start(),
    Timer = erlang:send_after(TimeInt div 2, self(), tick),
    {noreply, State#state{timer=Timer}};
handle_cast({result, Namespace, Aim, MetricName, Result},
            State) ->
    lager:info("Got result from ~s:~s[~s]", [Namespace, Aim,
                                             MetricName]),
    lager:debug("~s:~s[~s] result: ~p", [Namespace, Aim,
                                         MetricName, Result]),
    metrics_store:put({Namespace, Aim, MetricName}, Result),
    {noreply, State};
handle_cast(Msg, State) ->
    lager:debug("Got: ~p", [Msg]),
    {noreply, State}.

handle_info(tick, State=#state{cb_module=Cb,
                               aim_name=Name,
                               metrics=Metrics,
                               timer=OldTimer,
                               timer_int=TimeInt}) ->
    lager:debug("~s tick", [Name]),
    erlang:cancel_timer(OldTimer),
    SelfPid = self(),
    [spawn(fun() -> Res = Cb:get_metrics(MetricName),
                    gen_server:cast(SelfPid, Res)
           end) || MetricName <- Metrics],
    NewTimer = erlang:send_after(TimeInt, self(), tick),
    {noreply, State#state{timer=NewTimer}};
handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
