-module(import).
-export([
    terminal/3,
    file/3
]).

process(Input, Server, Manager) ->
    Transform = gen_server:call(Server, Input),
    gen_event:notify(Manager, Transform).

terminal(Data, Server, Manager) ->
    lists:foreach(fun(Input) ->
        process(Input, Server, Manager)
    end, Data).

file(Path, Manager, Server) ->
    {ok, Device} = file:open(Path, read),
    read_lines(Device, Manager, Server).

read_lines(Device, Manager, Server) ->
    case file:read_line(Device) of
        {ok, Line} ->
            Input = check_lines(Line),
            process(Input, Server, Manager),
            read_lines(Device, Manager, Server);
        eof ->
            ok = file:close(Device)
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
