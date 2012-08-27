-module(aim).

-behaviour(gen_server).

-export([start_link/5]).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-define(SERVER, ?MODULE).

-record(state, {tab,
                cb_module,
                metrics,
                aim_name}).

start_link(Name, Tid, Cb, Metrics, TimeInterval) ->
    gen_server:start_link({local, Name},
                          ?MODULE,
                          [Name, Tid, Cb, Metrics, TimeInterval],
                          []).

init([Name, Tid, Cb, Metrics, TimeInterval]) ->
    gen_server:cast(self(), {start, TimeInterval}),
    {ok, #state{tab=Tid, cb_module=Cb, metrics=Metrics,
                aim_name=Name}}.

handle_call(_Request, _From, State) ->
    {reply, ok, State}.

handle_cast({start, TimeInt}, State) ->
    inets:start(),
    timer:send_interval(TimeInt, tick),
    {noreply, State};
handle_cast({result, Metric, MetricName, Result},
            State=#state{tab=TId}) ->
    lager:info("Got result from ~s[~s]", [Metric, MetricName]),
    lager:debug("~s[~s] result: ~s", [Metric, MetricName,
                                     jsx:encode(Result)]),
    ets:insert(TId, {?MODULE, Result}),
    {noreply, State};
handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(tick, State=#state{cb_module=Cb,
                               aim_name=Name,
                               metrics=Metrics}) ->
    lager:debug("~s tick", [Name]),
    % Получать метрики из переданого при инициализации списка.
    [Cb:get_metrics(MetricName, self()) || MetricName <- Metrics],
    {noreply, State};
handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
