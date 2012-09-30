-module(metrics_store_tests).
-include_lib("eunit/include/eunit.hrl").

metrics_store_test_() ->
    {foreach,
     fun start/0,
     fun stop/1,
     [fun simple_store_test/1,
      fun amazon_store_test/1]}.

start() ->
    ets:new(metrics, [named_table, set]),
    metrics_store:put({"Namespace1", "test", "metric1"},
                      dict:from_list([{"key1", {1, "Count"}}])),
    metrics_store:put({"Namespace2", "test", "metric2"},
                      dict:from_list([{"key1", {2, "Count"}}])),
    metrics_store:put({"Namespace1", "test", "metric3"},
                      dict:from_list([{"key1", {3, "Count"}},
                                      {"key2", {5, "Count"}}])),
    metrics_store:put({"Namespace1", "test", "metric4"},
                      dict:from_list([{"key1", {4, "Count"}}])).

stop(_) ->
    ets:delete(metrics).

simple_store_test(_) ->
    F = fun (Key, Value) ->
                {Ns, _, M1} = Key,
                ValueList = dict:to_list(Value),
                F1 = fun ({K, V}) ->  {M1 ++ "_" ++ K, V} end,
                ValueList1 = lists:map(F1, ValueList),
                {Ns, ValueList1}
        end,

    Res = metrics_store:get_metrics(F),
    [?_assertEqual(
        [{"metric2_key1",{2,"Count"}}],
        dict:fetch("Namespace2", Res)),
     ?_assertEqual(
        lists:sort([{"metric1_key1",{1,"Count"}},
                    {"metric4_key1",{4,"Count"}},
                    {"metric3_key1",{3,"Count"}},
                    {"metric3_key2",{5,"Count"}}]),
        lists:sort(dict:fetch("Namespace1", Res)))].

-include_lib("erlcloud/include/erlcloud.hrl").
-include_lib("erlcloud/include/erlcloud_mon.hrl").

-define(M(N, V, U), #metric_datum{metric_name=N,
                                  value=V,
                                  unit=U,
                                  dimensions=[]}).

amazon_store_test(_) ->
    Res = metrics_store:get_metrics(
            fun amazon_cloudwatch_pusher:to_amazon_metrics/2
           ),
    [?_assertEqual([?M("key1", 1, "Count"),
                    ?M("key1", 4, "Count"),
                    ?M("key1", 3, "Count"),
                    ?M("key2", 5, "Count")],
                   dict:fetch("Namespace1", Res)),
     ?_assertEqual([?M("key1", 2, "Count")],
                   dict:fetch("Namespace2", Res))].
