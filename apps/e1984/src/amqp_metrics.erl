-module(amqp_metrics).

-export([pre_start/0, get_metrics/1]).
-define(FILE_DESCRIPTORS, "Free file descriptors for node ").
-define(NAMESPACE, "CustomMetrics: AMQP").

pre_start() ->
    case inets:start() of
        ok -> ok;
        {error, {already_started, inets}} -> ok;
        Error -> Error
    end.

get_metrics(nodes) ->
    get_nodes();
get_metrics(_Name) ->
    {error, unsuported}.

process(Body) ->
    DBody = jsx:decode(Body),
    lists:foldl(fun process_node/2, dict:new(), DBody).

get_nodes() ->
    {ok, {Login, Pwd}} = application:get_env(rabbit_conf),
    Cred = Login ++ ":" ++ Pwd,
    Auth = "Basic " ++ base64:encode_to_string(Cred),
    ReceiverPid = self(),
    {ok, _RequestId} = httpc:request(get,
                                     {"http://localhost:55672/api/nodes",
                                      [{"Authorization", Auth},
                                       {"Content-Type", "application/json"}
                                      ]
                                     },
                                     [],
                                     [{sync, false},
                                      {receiver, ReceiverPid}
                                     ]
                                    ),
    receive
        {http, {_Ref, Response}} ->
            {_St, _Hdrs, Body} = Response,
            Result = process(Body),
            {result, ?NAMESPACE, ?MODULE, nodes, Result}
    after 10000 ->
            exit(timeout)
    end.

process_node(Node, Acc) ->
    NodeName = proplists:get_value(<<"name">>, Node),
    FdTotal = proplists:get_value(<<"fd_total">>, Node),
    FdUsed = proplists:get_value(<<"fd_used">>, Node),
    FdFree = float(FdTotal - FdUsed),
    dict:store(?FILE_DESCRIPTORS ++ binary_to_list(NodeName), {FdFree, "Count"}, Acc).
