-module(test_metrics).

-export([get/2]).

get(nodes, Pid) ->
    cast_back(Pid, [{<<"test">>, <<"Test">>}]).

cast_back(Pid, Result) ->
    gen_server:cast(Pid, {result, ?MODULE, Result}).
