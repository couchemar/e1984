-module(amqp_aim).

-behaviour(gen_server).

-export([start_link/2]).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-define(SERVER, ?MODULE).

-record(state, {tab, sup}).


start_link(Tid, Sup) ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [Tid, Sup], []).

init([Tid, Sup]) ->
    gen_server:cast(self(), start),
    {ok, #state{tab=Tid, sup=Sup}}.

handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.

handle_cast(start, State) ->
    io:format("Started~n", []),
    inets:start(),
    timer:send_interval(5000, tick),
    {noreply, State};

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(tick, State) ->
    io:format("Tick~n", []),
    request(),

    {noreply, State};
handle_info({http, {_Ref, Response}}, State) ->
    io:format("Get Response ~n", []),
    {_St, _Hdrs, Body} = Response,
    Result = process_body(Body),
    io:format("Result: ~s~n", [jsx:encode(Result)]),
    Result,

    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

request() ->
    Auth = "Basic " ++ base64:encode_to_string("guest:guest"),
    {ok, _RequestId} = httpc:request(get,
                                     {"http://localhost:55672/api/nodes",
                                      [{"Authorization", Auth},
                                       {"Content-Type", "application/json"}]},
                                     [],
                                     [{sync, false},
                                     {receiver, self()}]).

process_body(Body) ->
    DBody = jsx:decode(Body),
    lists:foldl(fun process_node/2, [], DBody).

process_node(Node, Acc) ->
    NodeName = proplists:get_value(<<"name">>, Node),
    FdTotal = proplists:get_value(<<"fd_total">>, Node),
    FdUsed = proplists:get_value(<<"fd_used">>, Node),
    FdFree = FdTotal - FdUsed,
    Acc ++ [[{<<"node_name">>, NodeName}, {<<"fd_free">>, FdFree}]].

