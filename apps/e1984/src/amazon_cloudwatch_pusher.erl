-module(amazon_cloudwatch_pusher).

-behaviour(gen_server).

-export([start_link/1]).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-export([to_amazon_metrics/2]).

-include_lib("erlcloud/include/erlcloud.hrl").
-include_lib("erlcloud/include/erlcloud_mon.hrl").

-define(SERVER, ?MODULE).

start_link(TimeInterval) ->
    gen_server:start_link({local, ?SERVER}, ?MODULE,
                          [TimeInterval], []).

init([TimeInterval]) ->
    gen_server:cast(self(), {start, TimeInterval}),
    {ok, state}.

handle_call(_Request, _From, State) ->
    {reply, ok, State}.

handle_cast({start, TimeInt}, State) ->
    amazon_prepare(),
    timer:send_interval(TimeInt, tick),
    {noreply, State};
handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(tick, State) ->
    lager:debug("Tick"),
    MetricsDict = metrics_store:get_metrics(fun to_amazon_metrics/2),
    lager:debug("Metrics: ~p", [MetricsDict]),
    dict:map(
      fun (NameSpace, MetricValues) ->
              erlcloud_mon:put_metric_data(NameSpace, MetricValues)
      end,
      MetricsDict
     ),
    {noreply, State};
handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

amazon_prepare() ->
    erlcloud:start(),
    {ok, {Id, Secret}} = application:get_env(ec2_conf),
    erlcloud_ec2:configure(Id, Secret),
    {ok, {Host, Port, Scheme}} = application:get_env(mon_conf),
    erlcloud_mon:configure_host(Host, Port, Scheme).

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
