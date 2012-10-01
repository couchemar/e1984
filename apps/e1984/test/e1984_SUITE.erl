-module(e1984_SUITE).

-compile(export_all).

-include_lib("common_test/include/ct.hrl").

init_per_suite(Config) ->
    inets:start(),
    {ok, Server} = inets:start(
                     httpd,
                     [{port, 9998},
                      {server_name, "test.test"},
                      {server_root, "/tmp"},
                      {document_root, "/tmp"},
                      {modules, [?MODULE]}]),
    [{http_server, Server} | Config].

end_per_suite(Config) ->
    inets:stop(httpd, ?config(http_server, Config)),
    inets:stop(),
    ok.

all() ->
    [cloudwatch_pushers].

init_per_testcase(cloudwatch_pushers, Config) ->
    ets:new(metrics, [named_table, set]),
    metrics_store:put({"Namespace1", "test", "metric1"},
                      dict:from_list([{"key1", {1.0, "Count"}}])),
    amazon_cloudwatch_pusher:start_link(100),
    Config;
init_per_testcase(_, Config) ->
     Config.

end_per_testcase(cloudwatch_pushers, _Config) ->
    ets:delete(metrics);
end_per_testcase(_, _Config) ->
    ok.

cloudwatch_pushers(_Config) ->
    ct:sleep(500),
    ok.


%=====================================================================
% httpd callbacks
%=====================================================================

-include_lib("inets/include/httpd.hrl").

do(_ModData) ->
    {proceed,
     [{response, {200, [<<"OK!">>]}}]}.