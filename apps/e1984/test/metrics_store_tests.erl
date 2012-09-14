-module(metrics_store_tests).
-include_lib("eunit/include/eunit.hrl").

metrics_store_test_() ->
    {setup,
     fun start/0,
     fun store_test/1}.

start() ->
    ets:new(metrics, [named_table, set]).

store_test(_) ->
    metrics_store:put(1, 4),
    metrics_store:put(2, 3),
    metrics_store:put(3, 2),
    metrics_store:put(4, 1),

    [?_assertEqual(
        [{4, 1}, {3, 2}, {2, 3}, {1, 4}],
        metrics_store:get_metrics(fun (K, V) -> {V+10, K+100} end))].
