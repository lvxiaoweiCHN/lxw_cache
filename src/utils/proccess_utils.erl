-module (proccess_utils).
-export ([is_pid_alive/1]).
%% 判断某个进程是否存在
%% lists:member(Elem, List) -> boolean()
%% List中至少有一个元素和Elem相匹配，返回true
is_pid_alive(Pid) when node(Pid) =:= node() ->
    is_process_alive(Pid);
is_pid_alive(Pid) ->
    lists:member(node(Pid), nodes()) andalso
    (rpc:call(node(Pid), erlang, is_process_alive, [Pid]) =:= true).
