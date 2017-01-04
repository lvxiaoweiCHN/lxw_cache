-module (test_cache).
-include ("store.hrl").
-compile(export_all).

get_current_value(Key) ->
    {Key, Key}.

test_looger() ->
    ?report ("report ~p~n", ["测试1"]),
    ?warning ("warning ~p~n", ["测试1"]),
    ?error ("error ~p~n", ["测试1"]),
    ?report ("report ~p~n", ["测试2"]),
    ?warning ("warning ~p~n", ["测试2"]),
    ?error ("error ~p~n", ["测试2"]).
