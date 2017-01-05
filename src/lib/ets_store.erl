-module(ets_store).

%%使用ets表实现存储。ETS表的特点
%%    1. 无需在多个虚拟机之间共享，
%%    2. 需要持久化，且仅需要在VM运行期间持久化
%%    3. 由VM内的多个不同进程共享
%%    4. 访问速度要快
%%    5. 数据结构相对平坦，最好不要和其他表有外键关系
-export([init/0, insert/2, delete/1, lookup/1, delete_cache/1]).

-define(TABLE_ID, ?MODULE).

init() ->
    ets:new(?TABLE_ID, [public, named_table]),
    ok.

insert(Key, Value) ->
    Current_time = time_utils:get_current_seconds(),
    ets:insert(?TABLE_ID, {Key, {Value, Current_time}}).

lookup(Key) ->
    case ets:lookup(?TABLE_ID, Key) of
        [{Key, {Value, Time}}] ->
            {ok, Value, Time};
        [] ->
            {error, not_found}
    end.

delete(Key) ->
    ets:match_delete(?TABLE_ID, {Key, '_'}).

delete_cache(OutTime)->
    TimeTemp = time_utils:get_current_seconds() - OutTime,
    ets:fun2ms(fun({_Value, Current_time}) when Current_time < TimeTemp -> ets:match_delete(?TABLE_ID,{'_',Current_time}) end ).