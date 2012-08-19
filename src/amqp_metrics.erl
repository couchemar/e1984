-module(amqp_metrics).

-export([get/2, process/1]).

get(nodes, Params) ->
    get_nodes();
get(_Name, _Params) ->
    {error, unsuported}.

process(Body) ->
    DBody = jsx:decode(Body),
    lists:foldl(fun process_node/2, [], DBody).

get_nodes() ->
    Auth = "Basic " ++ base64:encode_to_string("guest:guest"),
    {ok, _RequestId} = httpc:request(get,
                                     {"http://localhost:55672/api/nodes",
                                      [{"Authorization", Auth},
                                       {"Content-Type", "application/json"}
                                      ]
                                     },
                                     [],
                                     [{sync, false},
                                      {receiver, self()}
                                     ]
                                    ).

process_node(Node, Acc) ->
    NodeName = proplists:get_value(<<"name">>, Node),
    FdTotal = proplists:get_value(<<"fd_total">>, Node),
    FdUsed = proplists:get_value(<<"fd_used">>, Node),
    FdFree = FdTotal - FdUsed,
    Acc ++ [[{<<"node_name">>, NodeName}, {<<"fd_free">>, FdFree}]].
