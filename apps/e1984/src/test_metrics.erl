-module(test_metrics).

-export([get_metrics/2]).

get_metrics(_, Pid) ->
    cast_back(Pid, [{<<"test">>, <<"Test">>}]).

cast_back(Pid, Result) ->
    gen_server:cast(Pid, {result, ?MODULE, test, Result}).
