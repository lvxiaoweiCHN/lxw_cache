-module(init_cache).
-export([start/0, init/0]).

start() ->
    application:start(lxw_cache).

init() ->
    config_utils:load_conf("store.conf"),
    lc_store:init(),
    ok.
