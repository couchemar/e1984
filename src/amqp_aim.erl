-module(amqp_aim).

-behaviour(gen_server).

-export([start_link/1]).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-define(SERVER, ?MODULE).

-record(state, {tab}).

start_link(Tid) ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [Tid], []).

init([Tid]) ->
    gen_server:cast(self(), start),
    {ok, #state{tab=Tid}}.

handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.

handle_cast(start, State) ->
    inets:start(),
    timer:send_interval(5000, tick),
    {noreply, State};

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(tick, State) ->
    amqp_metrics:get(nodes, []),
    {noreply, State};
handle_info({http, {_Ref, Response}}, State=#state{tab=TId}) ->
    {_St, _Hdrs, Body} = Response,
    Result = amqp_metrics:process(Body),
    ets:insert(TId, {?MODULE, Result}),
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
