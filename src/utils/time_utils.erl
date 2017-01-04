-module (time_utils).
-compile(export_all).

get_current_seconds() ->
    calendar:datetime_to_gregorian_seconds(calendar:local_time()).

verify_overtime(_Old_time, nolimit) ->
    nolimit;
verify_overtime(Old_time, Overtime) ->
    case Overtime - (get_current_seconds() - Old_time) of
        Time when Time =< 0 ->
            overtime;
        _ ->
            nolimit
    end.
