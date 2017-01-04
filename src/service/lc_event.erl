%% 在OTP中，事件管理器是一个事件可以发送到的命名对象，一个事件可以是一个错误、一个警告、或者一些要写入日志的信息
%% 在事件管理器中，有0个、一个或者多个事件处理器被安装，当事件管理器被一个事件通知时，这个事件将被安装在事件管理器中的事件处理器处理，
%% 事件管理器用一个进程实现，事件处理器用回调模块实现。事件管理器本质上维护一个{Module, State}列表，
%% 每一个Module为一个事件处理器， 而State为事件处理器的内部状态。
-module(lc_event).

-export([
            start_link/0,
            start/0,
            add_handler/2,
            add_sup_handler/2,
            call/2,
            delete_handler/2,
            notify/1,
            sync_notify/1,
            swap_handler/2,
            swap_sup_handler/2,
            stop/0,
            which_handlers/0
        ]).

-define(SERVER, ?MODULE).

%% 启动gen_evnet管理器，并以当前模块名注册进程名
%% 监控树可以通过调用该方法创建一个事件管理器进程作为一颗监控树的一部分，并连接到监控树。
start_link() ->
    gen_event:start_link({local, ?SERVER}).

%% 创建一个独立与监控树的事件管理器。其参数与start_link/0,1一样。
start() ->
    gen_event:start({local, ?SERVER}).

%% 向事件管理器添加一个事件处理器，事件管理器将通过回调Module:init/1初始化事件处理器和它的内部状态。
add_handler(Handler, Args) ->
    gen_event:add_handler(?SERVER, Handler, Args).

%% 与add_handler/3同样的方式添加一个新的事件处理器，并且与调用用进程间建立监控。
%% 如果调用进程因为原因Reason终止，事件管理器将回调Module:terminate/2删除该事件处理器,并以{stop, Reason}作为参数。
%% 如果事件处理器被删除，事件管理器将发送一个消息{gen_event_EXIT，Handler,Reason}给调用进程。
add_sup_handler(Handler, Args) ->
    gen_event:add_sup_handler(?SERVER, Handler, Args).

%% 向事件管理器EventMgrRef的事件处理器Handler发送一个同步请求Request,然后等待返回，
%% 或直到请求超时。事件管理器将调用Module:handle_call/2处理这个请求。
call(Handler, Request) ->
    gen_event:call(?SERVER, Handler, Request).

%% 从事件管理器中删除一个事件处理器Handler。事件管理器将调用Module:terminate/2来终止这个事件处理器。
delete_handler(Handler, Args) ->
    gen_event:delete_handler(?SERVER, Handler, Args).

%% 发送一个事件通知给事件管理器.这个事件管理器将调用每个安装事件处理器的Module:handle_event/2处理这个事件。
notify(Event) ->
    gen_event:notify(?SERVER, Event).
sync_notify(Event) ->
    gen_event:sync_notify(?SERVER, Event).

%% 在事件管理器中，用一个新的事件处理器替换老的事件处理器。
%% 第一个老的事件处理器被删除。事件管理器调用Module1:terminate(Args1, ...)删除事件处理器并保存其返回值
%% 然后新的事件处理器Handler2被添加并通过Module2:init({Args2,Term})初始化，Term是Module1:terminate的返回值
swap_handler({Handler1,Args1}, {Handler2,Args2}) ->
    gen_event:swap_handler(?SERVER, {Handler1,Args1}, {Handler2,Args2}).

swap_sup_handler({Handler1,Args1}, {Handler2,Args2}) ->
    gen_event:swap_sup_handler(?SERVER, {Handler1,Args1}, {Handler2,Args2}).

%% 终止事件管理器EventMgrRef.在终止之前事件管理器将调用Module:terminate(stop, ...)终止安装的事件处理器。
stop() ->
    gen_event:stop(?SERVER).

%% 返回事件管理器EventMgrRef中所有安装的事件处理器。
which_handlers() ->
    gen_event:which_handlers(?SERVER).
