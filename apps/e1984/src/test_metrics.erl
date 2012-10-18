-module(test_metrics).

-export([pre_start/0, get_metrics/1]).
-define(NAMESPACE, "CustomMetrics: TEST").

pre_start() ->
    ok.

get_metrics(_) ->
    Result = dict:from_list([{"test", {1.0, "Test"}}]),
    {result, ?NAMESPACE, ?MODULE, test, Result}.
