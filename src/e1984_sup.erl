
-module(e1984_sup).

-behaviour(supervisor).

-export([start_link/0]).

-export([init/1]).

-define(AIM(Name, Table, Cb, TimeInterval),
        {Name, {aim, start_link, [Name, Table, Cb, TimeInterval]},
         permanent, 5000, worker, [aim]}).
-define(PUSHER(I, Type, Table, TimeInterval),
        {I, {I, start_link, [Table, TimeInterval]},
         permanent, 5000, Type, [I]}).

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->
    ?MODULE = ets:new(?MODULE, [set, named_table, public]),
    AIMS = [?AIM(aim_amqp,
                 ?MODULE,
                 amqp_metrics,
                 5000),
            ?AIM(aim_test,
                 ?MODULE,
                 test_metrics,
                 10000)
           ],
    PUSHERS = [?PUSHER(amazon_cloudwatch_pusher, worker, ?MODULE, 20000)],
    {ok, { {one_for_one, 5, 10},
           AIMS ++ PUSHERS
         }
    }.


