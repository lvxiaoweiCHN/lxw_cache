-module (lc_server).
-behaviour (gen_server).
-include ("store.hrl").

-export([start_link/0, insert/2, lookup/1, delete/1]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

%%子进程状态
-record(state, {overtime, update_callback}).

%% 启动gen_server进程
start_link() ->
    gen_server:start_link(?MODULE, [], []).

insert(Key, Value) ->
    io:format("insert cast ~p,~p~n", [Key, Value]),
    gen_server:cast(?MODULE, {insert, Key, Value}).

lookup(Key) ->
    io:format("lookup call ~p~n~n", [Key]),
    gen_server:call(?MODULE, {lookup, Key}).

delete(Key) ->
    io:format("delete cast ~p~n~n", [Key]),
    gen_server:cast(?MODULE, {delete, Key}).

% --------------------------------------------------------------------
% Function: init/1
% Description: Initiates the server
% Returns: {ok, State}          |
%          {ok, State, Timeout} |
%          ignore               |
%          {stop, Reason}
% --------------------------------------------------------------------
init([]) ->
    Update_callback = ?config("update_callback"),
    Overtime = ?config("cache_time", 5),
    {ok, #state{overtime = Overtime, update_callback = Update_callback}}.

% --------------------------------------------------------------------
% Function: handle_call/3
% Description: Handling call messages
% Returns: {reply, Reply, State}          |
%          {reply, Reply, State, Timeout} |
%          {noreply, State}               |
%          {noreply, State, Timeout}      |
%          {stop, Reason, Reply, State}   | (terminate/2 is called)
%          {stop, Reason, State}            (terminate/2 is called)
% --------------------------------------------------------------------
handle_call({lookup, Key}, _From, State) ->
    #state{overtime = Overtime, update_callback = {Module, Fun}} = State,
    io:format("lookup handle_call ~p~n", [Key]),
    {Value, Old_time} = lc_store:lookup(Key),
    case time_utils:verify_overtime(Old_time, Overtime) of
        nolimit ->
            {reply, {ok, Value}, State};
        overtime ->
            {NewKey, NewValue} = apply(Module, Fun, Key),
            spawn(?MODULE, update, [{NewKey, NewValue}]),
            {reply, {ok, NewValue}, State}
    end;
handle_call(_, _From, State) ->
    {noreply, State}.

% --------------------------------------------------------------------
% Function: handle_cast/2
% Description: Handling cast messages
% Returns: {noreply, State}          |
%          {noreply, State, Timeout} |
%          {stop, Reason, State}            (terminate/2 is called)
% --------------------------------------------------------------------
handle_cast({insert, Key, Value}, State) ->
    io:format("insert handle_cast ~p,~p~n", [Key, Value]),
    lc_store:insert(Key, Value),
    {noreply, State};
handle_cast({delete, Key}, State) ->
    io:format("insert handle_cast ~p~n", [Key]),
    lc_store:delete(Key),
    {noreply, State};
handle_cast(Request, State) ->
    io:format("Request handle_cast ~p~n", [Request]),
    {noreply, State}.
% --------------------------------------------------------------------
% Function: handle_info/2
% Description: Handling all non call/cast messages
% Returns: {noreply, State}          |
%          {noreply, State, Timeout} |
%          {stop, Reason, State}            (terminate/2 is called)
% --------------------------------------------------------------------
handle_info(_Info, State) ->
    {noreply, State}.

% --------------------------------------------------------------------
% Function: terminate/2
% Description: Shutdown the server
% Returns: any (ignored by gen_server)
% --------------------------------------------------------------------
terminate(_Reason, _State) ->
    ok.

% --------------------------------------------------------------------
% Func: code_change/3
% Purpose: Convert process state when code is changed
% Returns: {ok, NewState}
% --------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
