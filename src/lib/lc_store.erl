%% 存储中间人
-module(lc_store).
-include ("store.hrl").
-compile(export_all).

%% 方法反回 ok
init() ->
    apply(getImplementModule(), init, []).

%% 方法反回 {ok, Value, Time} | {error, not_found}
lookup(Key) ->
    io:format("lookup lc_store ~p~n", [Key]),
    ?report ("lookup ~p~n", [Key]),
    apply(getImplementModule(), lookup, [Key]).

%% 方法返回 true | false
insert({Key, Value}) ->
    io:format("insert lc_store ~p,~p~n", [Key, Value]),
    ?report ("insert ~p,~p~n", [Key,Value]),
    apply(getImplementModule(), insert, [Key, Value]).

%% 方法返回 true | false
delete(Key) ->
    io:format("delete lc_store ~p~n", [Key]),
    ?report ("delete ~p~n", [Key]),
    apply(getImplementModule(), delete, [Key]).

delete_cache(OutTime)->
    ets_store:delete_cache(OutTime).

getImplementModule() ->
    case config_utils:get(store_type) of
        {M, F} ->
            apply(M, F, []);
        Store_type ->
            list_to_atom(lists:concat([Store_type, "_", "store"]))
    end.
