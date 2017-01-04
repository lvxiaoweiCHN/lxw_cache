-module(config_utils).
-include ("store.hrl").

-compile(export_all).

put(Key, Value) ->
    module_store:insert(key_to_key(Key), Value).

get(Key) ->
    get(Key, []).

get(Key, Default) ->
    case module_store:lookup(key_to_key(Key)) of
        {ok, Value, _} ->
            Value;
        _ ->
            Default
    end.

key_to_key(Key) ->
    list_to_atom(lists:concat(["config", "_", Key])).

load_conf(File) ->
    [?MODULE:put(Key, Value) || {Key, Value} <- consult(File)],
    ok.

consult(File) ->
    case file:consult(get_file_path(File)) of
        {ok, Content} ->
            Include_Files = [In_file_name || {Key, In_file_name} <- Content, Key =:= include],
            lists:foldl(
              fun(File_name, Acc) ->
                  consult(get_file_path(File_name)) ++ Acc
              end, Content, Include_Files);
        {error, enoent} ->
            throw("Can not find config file " ++ File);
        {error, Error} ->
            throw({error, Error});
        Other ->
            throw({error, Other})
    end.

get_file_path(File) ->
    lists:concat([?APP_PATH, "config/", File]).
