-module(metrics_store_tests).
-include_lib("eunit/include/eunit.hrl").

metrics_store_test_() ->
    {setup,
     fun start/0,
     fun store_test/1}.

start() ->
    ets:new(metrics, [named_table, set]).

store_test(_) ->
    metrics_store:put({"Namespace1", "test", "metric1"},
                      dict:from_list([{"key1", {1, "Count"}}])),
    metrics_store:put({"Namespace2", "test", "metric2"},
                      dict:from_list([{"key1", {2, "Count"}}])),
    metrics_store:put({"Namespace1", "test", "metric3"},
                      dict:from_list([{"key1", {3, "Count"}},
                                      {"key2", {5, "Count"}}])),
    metrics_store:put({"Namespace1", "test", "metric4"},
                      dict:from_list([{"key1", {4, "Count"}}])),

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
        lists:sort(dict:fetch("Namespace1", Res)))
    ].

