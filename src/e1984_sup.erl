
-module(e1984_sup).

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

%% Helper macro for declaring children of supervisor
-define(CHILD(I, Type, T, S), {I, {I, start_link, [T, S]}, permanent, 5000, Type, [I]}).

%% ===================================================================
%% API functions
%% ===================================================================

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%% ===================================================================
%% Supervisor callbacks
%% ===================================================================

init([]) ->
    ?MODULE = ets:new(?MODULE, [set, named_table, private]),
    {ok, { {one_for_one, 5, 10}, [?CHILD(amqp_aim, worker, ?MODULE, self())]} }.

