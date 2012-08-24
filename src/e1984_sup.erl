
-module(e1984_sup).

-behaviour(supervisor).

-export([start_link/0]).

-export([init/1]).

-define(AIM(I, Type, Table, Cb, TimeInterval),
        {I, {I, start_link, [Table, Cb, TimeInterval]},
         permanent, 5000, Type, [I]}).
-define(PUSHER(I, Type, Table, TimeInterval),
        {I, {I, start_link, [Table, TimeInterval]},
         permanent, 5000, Type, [I]}).

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->
    ?MODULE = ets:new(?MODULE, [set, named_table, public]),
    AIMS = [?AIM(aim, worker,
                   ?MODULE,
                   amqp_metricks,
                   5000)
           ],
    PUSHERS = [?PUSHER(amazon_cloudwatch_pusher, worker, ?MODULE, 20000)],
    {ok, { {one_for_one, 5, 10},
           AIMS ++ PUSHERS
         }
    }.


