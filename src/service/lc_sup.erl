-module(lc_sup).
-athor(lvxiaowei).
-behaviour(supervisor).

-export([start_link/0, init/1]).
-define(SERVER, ?MODULE).

%% 在本地节点上注册一个名为 ?SERVER 的子进程:根监督树
start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

init(_Args) ->
    %% 子进程规范{ID, Start, Restart, Shutdown, Type, Modules}
    %% 监督者的子进程被标识为temporary：子进程退出后无需重启
    %% 子进程的关闭策略是brutal_kill:子进程应随监督者的关闭而立即终止
    %% permanent型，子进程在间隔指定的时间后重启
    ElementSup = {lc_server, {lc_server, start_link, []}, permanent, 2000, worker, [lc_server]},
    EventManager = {lc_event, {lc_event, start_link, []}, permanent, 2000, worker, [lc_event]},
    CacheClean = {lc_clean, {lc_clean, start_link, []}, permanent, 2000, worker, [lc_clean]},
    Children = [ElementSup, EventManager,CacheClean],
    %% 重启策略，one_for_one监督策略，可以同时监督多个不同类型的子进程
    RestartStrategy = {one_for_one, 4, 3600},
    {ok, {RestartStrategy, Children}}.
