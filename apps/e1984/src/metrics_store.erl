-module(metrics_store).

-export([put/2, get_metrics/1]).

-include_lib("stdlib/include/qlc.hrl").

-define(METRICS, metrics).

put(Key, Metric) ->
    ets:insert(?METRICS, {Key, Metric}).

%% @doc: Возвращает все накопленые метрики в виде словаря.
%% Функция Transform должна возвращать ключ и значение которое
%% попадет в словарь.
get_metrics(Transform) ->
    TrMetrics = qlc:q([Transform(K, V) || {K,V} <- ets:table(?METRICS)]),
    MetricsList = qlc:e(TrMetrics),
    F = fun (Item, Acc) ->
                {Key, Value} = Item,
                dict:append_list(Key, Value, Acc)
        end,
    lists:foldl(F, dict:new(), MetricsList).
