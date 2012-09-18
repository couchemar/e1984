-module(metrics_store).

-export([put/2, get_metrics/1]).

-include_lib("stdlib/include/qlc.hrl").

-define(METRICS, metrics).

put(Key, Metric) ->
    ets:insert(?METRICS, {Key, Metric}).

%% @doc: Возвращает все накопленые метрики в виде словаря.
%% Функция Transform должна возвращать ключ и значение которе
%% попадет в словарь.
get_metrics(Transform) ->
    R = qlc:q([Transform(K, V) || {K,V} <- ets:table(?METRICS)]),
    qlc:e(R).
