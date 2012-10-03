-module(e1984_sup).

-behaviour(supervisor).

-export([start_link/0]).

-export([init/1]).

-define(AIM(Name, Cb, Metrics, TimeInterval),
        {Name, {aim, start_link, [Name, Cb, Metrics, TimeInterval]},
         permanent, 5000, worker, [aim]}).
-define(PUSHER(I, TimeInterval),
        {I, {I, start_link, [TimeInterval]},
         permanent, 5000, worker, [I]}).

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->
    ets:new(metrics, [set, named_table, public]),
    AIMS = [?AIM(aim_amqp,
                 amqp_metrics,
                 [nodes],
                 5000),
            ?AIM(aim_test,
                 test_metrics,
                 [test],
                 10000)
           ],
    PUSHERS = [?PUSHER(amazon_cloudwatch_pusher, 20000)],
    {ok, { {one_for_one, 5, 10},
           AIMS ++ PUSHERS
         }
    }.
