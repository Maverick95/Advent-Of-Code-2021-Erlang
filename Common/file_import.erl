-module(file_import).

-export([
    import/2
]).



import(Path, Manager) ->
    case file:open(Path, read) of
        {ok, Device} ->
            read_lines(Device, Manager);
        _ ->
            error
    end.



read_lines(Device, Manager) ->
    case file:read_line(Device) of
        {ok, Line} ->
            Data = check_lines(Line),
            gen_event:notify(Manager, Data),
            read_lines(Device, Manager);
        {error, _} ->
            error;
        eof ->
            case file:close(Device) of
                ok ->
                    ok;
                _ ->
                    error
            end
    end.



check_lines(Line) ->
    LineReverse = reverse(Line),
    case check_first(LineReverse, 10) of
        true ->
            [_ | Rest] = LineReverse,
            reverse(Rest);
        false ->
            Line
    end.



reverse(Line) ->
    reverse(Line, []).

reverse([], Current) ->
    Current;

reverse([Head | Rest], Current) ->
    reverse(Rest, [Head | Current]).



check_first([], _) ->
    false;

check_first([Head | _], Char) ->
    case Head of
        Char ->
            true;
        _ ->
            false
    end.


