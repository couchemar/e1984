-module(amazon_cloudwatch_pusher).

-behaviour(gen_server).

-export([start_link/2]).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-define(SERVER, ?MODULE).

-record(state, {tab}).

start_link(Tid, TimeInterval) ->
    amazon_prepare(),
    gen_server:start_link({local, ?SERVER}, ?MODULE,
                          [Tid, TimeInterval], []).

init([Tid, TimeInterval]) ->
    gen_server:cast(self(), {start, TimeInterval}),
    {ok, #state{tab=Tid}}.

handle_call(_Request, _From, State) ->
    {reply, ok, State}.

handle_cast({start, TimeInt}, State) ->
    timer:send_interval(TimeInt, tick),
    {noreply, State};
handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(tick, State) ->
    lager:debug("Tack"),
    {noreply, State};
handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

amazon_prepare() ->
    erlcloud:start(),
    erlcloud_ec2:configure("11111111111111111111",
                           "2222222222222222222222222222222222222222"),
    erlcloud_mon:configure_host("localhost", "9999", "http").
