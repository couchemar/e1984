-module(aim).

-behaviour(gen_server).

-export([start_link/3]).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-define(SERVER, ?MODULE).

%% Интервал тика держать в состоянии совсем не обязательно,
%% и даже наверное стоит его оттуда убрать. но не сейчас.
-record(state, {tab, cb_module, interval}).

start_link(Tid, Cb, TimeInterval) ->
    gen_server:start_link({local, ?SERVER},
                          ?MODULE,
                          [Tid, Cb, TimeInterval],
                          []).

init([Tid, Cb, TimeInterval]) ->
    gen_server:cast(self(), start),
    {ok, #state{tab=Tid,
                cb_module=Cb,
                interval=TimeInterval}}.

handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.

handle_cast(start, State=#state{interval=TimeInt}) ->
    inets:start(),
    timer:send_interval(TimeInt, tick),
    {noreply, State};
handle_cast({result, Result}, State=#state{tab=TId}) ->
    ets:insert(TId, {?MODULE, Result}),
    {noreply, State};
handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(tick, State) ->
    io:format("tick"),
    amqp_metrics:get(nodes, self()),
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
