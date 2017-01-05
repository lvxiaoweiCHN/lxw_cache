-module(lc_clean).
-author("tagore").

-behaviour(gen_server).
-include ("store.hrl").
%% API
-export([start_link/0,set_time/2]).

%% gen_server callbacks
-export([init/1,
  handle_call/3,
  handle_cast/2,
  handle_info/2,
  terminate/2,
  code_change/3]).

-define(SERVER, ?MODULE).

-record(state, {check_time, out_time,callback}).

start_link() ->
  gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

init([]) ->
  OutTime = ?config("out_time"),
  CheckTime = ?config("check_time", 5),
  {ok, #state{check_time = CheckTime, out_time = OutTime,callback=?config(store_type)},CheckTime}.

set_time(CheckTime,OutTime)->
  gen_server:call(?SERVER,{set_time,CheckTime,OutTime}).

handle_call({set_time,CheckTime,OutTime}, _From, _State= #state{check_time = _, out_time = _,callback = CallBack}) ->
      {reply, ok, #state{check_time = CheckTime, out_time = OutTime, callback = CallBack},CheckTime};

handle_call(_Request, _From, State) ->
  {reply, ok, State}.

handle_cast(_Request, State) ->
  {noreply, State}.

handle_info(timeout, #state{check_time=CheckTime, out_time=OutTime,callback = CallBack} = State) ->
	Call= list_to_atom(atom_to_list(CallBack) ++ "_out_time_delete"),
	clean_util:Call(OutTime),
	{noreply, State,CheckTime};

handle_info(_Info, State) ->
  {noreply, State}.

terminate(_Reason, _State) ->
  ok.

code_change(_OldVsn, State, _Extra) ->
  {ok, State}.


