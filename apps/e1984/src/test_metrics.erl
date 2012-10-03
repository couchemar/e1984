-module(test_metrics).

-export([pre_start/0, get_metrics/2]).
-define(NAMESPACE, "CustomMetrics: TEST").

pre_start() ->
    ok.

get_metrics(_, Pid) ->
    cast_back(Pid, dict:from_list([{"test", {1.0, "Test"}}])).

cast_back(Pid, Result) ->
    gen_server:cast(Pid, {result, ?NAMESPACE, ?MODULE, test, Result}).
