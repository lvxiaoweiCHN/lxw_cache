#!/bin/bash
echo "start cache ..."
#erl -detached -sname lxw_cache -id lxw_cache -pa ebin -setcookie lxw_cache_cookie  -s init_cache start
erl -sname lxw_cache -id lxw_cache -pa ebin -setcookie lxw_cache_cookie  -s init_cache start
if [ $? -eq 0 ]; then
    echo "start cache success"
else
    echo "start cache fail !"
fi
