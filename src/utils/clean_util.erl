%%%-------------------------------------------------------------------
%%% @author tagore
%%% @copyright (C) 2017, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 05. 一月 2017 21:17
%%%-------------------------------------------------------------------
-module(clean_util).
-author("tagore").

%% API
-export([ets_out_time_delete/1, mnesia_out_time_delete/1]).
ets_out_time_delete(OutTime)->
	ets_store:delete_cache(OutTime).

mnesia_out_time_delete(OutTime)->
	mnesia_store:delete_cache(OutTime).
