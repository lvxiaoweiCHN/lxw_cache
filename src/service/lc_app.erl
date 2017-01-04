%%自主运转能力的主动应用，应用行为模式的实现模块和根监督者
%%编译模块：erlc -o ebin src/*.erl
-module(lc_app).
-behaviour(application).
-export([start/2, stop/1]).

%%启动根监督者
start(_StartType, _StartArgs) ->
    init_cache:init(),
    %%启动根监督进程
    case lc_sup:start_link() of
        {ok, Pid} ->
            %% 注册日志处理器
            lc_logger:register(),
            {ok, Pid};
        Other ->
            {error, Other}
    end.

stop(_State) ->
    ok.
