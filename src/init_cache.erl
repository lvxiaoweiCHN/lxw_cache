-module(init_cache).
-export([init/0]).

init() ->
    config_utils:load_conf("store.conf"),
    lc_store:init(),
    ok.
