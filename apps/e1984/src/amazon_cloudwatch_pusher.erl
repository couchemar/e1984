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
    lager:debug("Tack"),
    %% Получить тут метрики в формате пригодном для отправки
    %% в амазон и запушить их.
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

to_amazon_metrics(Key, Value) ->
    {NameSpace, _, _} = Key,
    ValueList = dict:to_list(Value),
    F = fun ({MetricName, {Val, Unit}}) ->
                #metric_datum{
              metric_name = MetricName,
              unit = Unit,
              value = Val
             }
        end,
    ValueList1 = lists:map(F, ValueList),
    {NameSpace, ValueList1}.
