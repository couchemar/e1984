-module(amqp_metrics).

-export([get_metrics/2]).
-define(FILE_DESCRIPTORS, "Free file descriptors for node ").
-define(NAMESPACE, "CustomMetrics: AMQP").

get_metrics(nodes, Pid) ->
    get_nodes(Pid);
get_metrics(_Name, _Pid) ->
    {error, unsuported}.

process(Body) ->
    DBody = jsx:decode(Body),
    lists:foldl(fun process_node/2, dict:new(), DBody).

get_nodes(Pid) ->
    Auth = "Basic " ++ base64:encode_to_string("guest:guest"),
    ReceiverPid = spawn(fun() -> receive_nodes(Pid) end),
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
                                    ).

process_node(Node, Acc) ->
    NodeName = proplists:get_value(<<"name">>, Node),
    FdTotal = proplists:get_value(<<"fd_total">>, Node),
    FdUsed = proplists:get_value(<<"fd_used">>, Node),
    FdFree = FdTotal - FdUsed,
    dict:store(?FILE_DESCRIPTORS ++ NodeName, {FdFree, "Count"}, Acc).

receive_nodes(Pid) ->
    receive
        {http, {_Ref, Response}} ->
            {_St, _Hdrs, Body} = Response,
            Result = process(Body),
            cast_back(Pid, nodes, Result),
            exit(normal)
    after 10000 ->
            exit(timeout)
    end.

cast_back(Pid, Metric, Result) ->
    gen_server:cast(Pid, {result, ?MODULE, Metric, ?NAMESPACE, Result}).
