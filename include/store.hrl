-define(APP, lxw_cache).
-define(APP_PATH, "/root/workspace/lxw_cache/").

-define (config (Key), config_utils:get(Key)).
-define (config (Key, Default), config_utils:get(Key, Default)).
-define (config_put (Key, Value), config_utils:put(Key, Value)).

-define (report (Format, Msg), lc_event:notify({report, {"~p:~p " ++ Format, [?MODULE,?LINE]++Msg}})).
-define (warning (Format, Msg), lc_event:notify({warning, {"~p:~p " ++ Format, [?MODULE,?LINE]++Msg}})).
-define (error (Format, Msg), lc_event:notify({error, {"~p:~p " ++ Format, [?MODULE,?LINE]++Msg}})).
