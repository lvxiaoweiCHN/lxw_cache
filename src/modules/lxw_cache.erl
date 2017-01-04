-module(lxw_cache).

%% 用户接口
-export([insert/2, lookup/1, delete/1]).

insert(Key, Value) ->
    lc_server:insert(Key, Value).

lookup(Key) ->
    lc_server:lookup(Key).

delete(Key) ->
    lc_server:delete(Key).
