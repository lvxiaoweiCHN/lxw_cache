-module(lc_logger).
-bahavour(gen_event).
-include ("store.hrl").

%% 回调函数导出
-export([
            init/1,
            init/2,
            handle_event/2,
            handle_call/2,
            handle_info/2,
            terminate/2,
            code_change/3
        ]).

%% 用户接口导出
-export([
            register/0,
            remove/0
        ]).

-record(state, {report_file, warning_file, error_file}).

register() ->
    lc_event:add_handler(?MODULE, []).

remove() ->
    lc_event:delete_handler(?MODULE).

%% Module:init(InitArgs) -> {ok,State} | {ok,State,hibernate} | {error,Reason}
%% InitArgs = Args | {Args,Term}
%% gen_event:add_handler/3或gen_event:add_sup_handler/3来添加事件处理器。InitArgs以Args作为参数。
%% gen_event:swap_handler/3或gen_event:swap_sup_handler来替换另一个事件处理器，InitArgs以{Args,Term}作为参数.
%% 如果返回{ok,State,hibernate}，事件管理器将进入'hibernate'状态，并等待下一个事件发生。
init(_Args) ->
    File_names = ["report", "warning", "error"],
    File_paths = [ lists:concat([?APP_PATH, "log/", File_name, ".log"]) || File_name <- File_names ],
    Files = [ file_utils:open_file_append(File_path) || File_path <- File_paths ],
    {ok, #state{report_file=lists:nth(1,Files),
                warning_file=lists:nth(2, Files),
                error_file=lists:nth(3,Files)}}.

init(Args, _Term) ->
    init(Args).

%% gen_event:notify/2或gen_event:sync_notify/2发送过来的事件时，每个安装的事件处理器将调用该方法处理事件。
%% Module:handle_event(Event, State) -> Result
%% Result = {ok,NewState} | {ok,NewState,hibernate} | {swap_handler,Args1,NewState,Handler2,Args2} | remove_handler
%% 如果这个函数返回{ok, NewState}或{ok, NewState, hibernate}该事件处理器保持在事件管理器内部，并更新内部状态。
%% 如果任一事件管理器返回的是{ok,NewState,hibernate}，事件管理器将进入'hibernate'状态，等待下一个事件的产生。
%% 如果函数返回{swap_handler,Args1,NewState,Handler2,Args2}该事件处理器将被Handler2替换,
%% 通过调用Module:terminate(Args1,NewState)和Module2:init({Args2,Term})进行替换。详情参见gen_event:swap_handler/3.
%% 如果该函数返回remove_handler,该事件管理器将被通过调用Module:terminate(remove_handler,State)删除。
handle_event({report, {Format, Msg}}, State) ->
    #state{report_file = Report_file} = State,
    write_log(Report_file, Format, Msg),
    {ok, State};
handle_event({warning, {Format, Msg}}, State) ->
    #state{warning_file = Warning_file} = State,
    write_log(Warning_file, Format, Msg),
    {ok, State};
handle_event({error, {Format, Msg}}, State) ->
    #state{error_file = Error_file} = State,
    write_log(Error_file, Format, Msg),
    {ok, State};
handle_event(_Event, State) ->
    {ok, State}.

%% Module:handle_call(Request, State) -> Result
%% Result = {ok,Reply,NewState} | {ok,Reply,NewState,hibernate} |
%% {swap_handler,Reply,Args1,NewState,Handler2,Args2}|{remove_handler, Reply}
%% 每当一个事件管理器接收到来自gen_event:call/3,4的请求时，这个函数将被指定的事件处理器调用来处理这个请求。
%% 该函数的返回值参见handle_event/2,同时包含了一个任意的数据结构Reply,它将返回给调用端。
handle_call(_Request, State) ->
    {ok, State}.

%% Module:handle_info(Info, State) -> Result
%% Result = {ok,NewState} | {ok,NewState,hibernate} | {swap_handler,Args1,NewState,Handler2,Args2} | remove_handler
%% 当一个事件管理器接收到其他信息(除event,synchronous request,system message)每个安装的事件处理器都会调用该函数处理这个消息。
handle_info(_Info, State) ->
    {ok, State}.


%% Module:terminate(Arg, State) -> term()
%% Arg = Args | {stop,Reason} | stop | remove_handler | {error,{'EXIT',Reason}} | {error,Term}
%% Args = Reason = Term = term()
%% gen_event:delete_handler, gen_event:swap_handler/3 or gen_event:swap_sup_handler/3来删除，Arg就是他们调用时的参数信息Args.
%% 如果事件处理器相监控连接的进程由于Reason终止时，Arg ={stop, Reason}.
%% 如果由于事件管理器终止而终止事件处理器时，Arg = stop.
%% 如果事件管理器是监控树的一部分并被监控树按顺序终止。甚至它不是监控树的一部分，如过它接受到来自父进程的退出信号'EXIT'都将被终止。
%% 如果事件处理器被删除由于另一个回调函数返回了remove_handler or {remove_handler,Reply},Arg = remove_handler.
%% 如果事件处理器被删除是因为回调函数返回了一个意外值Term，Arg={error,Term};如果回调函数失败，Arg={error,{'EXIT',Reason}}.
%% 该函数将返回任意类型的值。如果事件处理器被删除是由于调用gen_event:delete_handler,那个函数的返回值就是这个函数的返回值。
%% 如果事件处理器被另一个时间处理器替换，返回值将被作为另一个事件处理器的初始化参数。否则，返回值将被忽略。
terminate(_Arg, State) ->
    #state{report_file = Report_file, warning_file = Warning_file, error_file = Error_file} = State,
    file:close(Report_file),
    file:close(Warning_file),
    file:close(Error_file),
    ok.

%% Module:code_change(OldVsn, State, Extra) -> {ok, NewState}
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

write_log(File, Format, Msg) ->
    try io:format(File, Format, Msg) of
        ok ->
            ok;
        _ ->
            io:format(File, "error ~p~n", [Msg])
    catch
        _:_ ->
            io:format(File, "error ~p~n", [Msg])
    end.
