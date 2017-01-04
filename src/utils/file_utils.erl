-module (file_utils).
-compile(export_all).

open_file_append(File) ->
    case file:open(File, [append]) of
        {ok,IoDevice} ->
            IoDevice;
        _ ->
            throw({error, "report log file open failed!"})
    end.
