-module(amqp_metrics).

-export([get/2]).

get(nodes, Pid) ->
    get_nodes(Pid);
get(_Name, _Pid) ->
    {error, unsuported}.

process(Body) ->
    DBody = jsx:decode(Body),
    lists:foldl(fun process_node/2, [], DBody).

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
    Acc ++ [[{<<"node_name">>, NodeName}, {<<"fd_free">>, FdFree}]].

receive_nodes(Pid) ->
    receive
        {http, {_Ref, Response}} ->
            {_St, _Hdrs, Body} = Response,
            Result = process(Body),
            cast_back(Pid, Result),
            exit(normal)
    after 10000 ->
            timeout
    end.

cast_back(Pid, Result) ->
    gen_server:cast(Pid, {result, ?MODULE, Result}).
