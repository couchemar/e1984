-module(amazon_cloudwatch_pusher).

-behaviour(gen_server).

-export([start_link/1]).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

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
    lager:debug("Tack"),
    Metrics = get_metrics(),
    dict:map(fun (Ns, M) ->
                     erlcloud_mon:put_metric_data(Ns, M) end,
             Metrics),
    {noreply, State};
handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

amazon_prepare() ->
    erlcloud:start(),
    erlcloud_ec2:configure("11111111111111111111",
                           "2222222222222222222222222222222222222222"),
    erlcloud_mon:configure_host("localhost", "9999", "http").

get_metrics() ->
    ets:foldl(fun get_metric/2, dict:new(), metrics).
get_metric(Item, Acc) ->
    {{_, _}, MetricDict} = Item,
    dict:fold(fun dict_record_to_metric/3, dict:new(), MetricDict),
    dict:append(MetricName, Metric, Acc).

dict_record_to_metric(Key, {Val, Unit, NameSpace}, Acc) ->
    Metric = #metric_datum{
      metric_name = Key,
      dimensions = [],
      statistic_values = undefined,
      timestamp = undefined,
      unit = Unit,
      value = Val
     },
    dict:append(NameSpace, Metric, Acc).




