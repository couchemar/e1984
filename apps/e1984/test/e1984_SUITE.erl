-module(e1984_SUITE).

-compile(export_all).

-include_lib("common_test/include/ct.hrl").

init_per_suite(Config) ->
    %{ok, Server} = inets:start(
    %                 httpd,
    %                 [{port, 9999},
     %%                 {server_name, "test.test"},
     %                 {server_root, "/tmp"},
      %                {documet_root, "/tmp"}],
       %              stand_alone),
    %e1984_app:start(),
    %[{http_server, Server} | Config].
    Config.

end_per_suite(_Config) ->
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


%
% httpd callbacks
%

