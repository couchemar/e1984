-module(amazon_cloudwatch_pusher).

-behaviour(gen_server).

-export([start_link/2]).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-export([to_amazon_metrics/2]).

-include_lib("erlcloud/include/erlcloud.hrl").
-include_lib("erlcloud/include/erlcloud_mon.hrl").

-define(SERVER, ?MODULE).

-record(state, {timer,
                timer_int}).


start_link(TimeInterval, Config) ->
    gen_server:start_link({local, ?SERVER}, ?MODULE,
                          [TimeInterval, Config], []).

init([TimeInterval, Config]) ->
    gen_server:cast(self(), {start, TimeInterval, Config}),
    {ok, #state{timer_int=TimeInterval}}.

handle_call(_Request, _From, State) ->
    {reply, ok, State}.

handle_cast({start, TimeInt, Config}, State) ->
    amazon_prepare(Config),
    Timer = erlang:send_after(TimeInt div 2, self(), tick),
    {noreply, State#state{timer=Timer}};
handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(tick, State=#state{timer=OldTimer,
                               timer_int=TimeInt}) ->
    erlang:cancel_timer(OldTimer),
    lager:debug("Tick"),
    MetricsDict = metrics_store:get_metrics(fun to_amazon_metrics/2),
    lager:debug("Metrics: ~p", [MetricsDict]),
    dict:map(
      fun (NameSpace, MetricValues) ->
              erlcloud_mon:put_metric_data(NameSpace, MetricValues)
      end,
      MetricsDict
     ),
    NewTimer = erlang:send_after(TimeInt, self(), tick),
    {noreply, State#state{timer=NewTimer}};
handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

amazon_prepare(Config) ->
    erlcloud:start(),
    case proplists:get_value(ec2_conf, Config) of
        {Id, Secret} -> erlcloud_ec2:configure(Id, Secret)
    end,
    case proplists:get_value(mon_conf, Config) of
        {Host, Port, Scheme} ->
            erlcloud_mon:configure_host(Host, Port, Scheme)
    end.

to_amazon_metrics(Key, Value) ->
    {NameSpace, _, _} = Key,
    ValueList = dict:to_list(Value),
    F = fun ({MetricName, {Val, Unit}}) ->
                #metric_datum{metric_name = MetricName,
                              dimensions  = [],
                              unit = Unit,
                              value = Val}
        end,
    ValueList1 = lists:map(F, ValueList),
    {NameSpace, ValueList1}.
