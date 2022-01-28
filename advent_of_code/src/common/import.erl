-module(import).
-export([
    terminal/2,
    file/2
]).

process(Input, Servers) ->
    [Transform | Parts] = Servers,
    Result = gen_server:call(Transform, Input),
    lists:foreach(fun(Part) ->
        gen_server:cast(Part, Result)
    end, Parts).

terminal(Data, Servers) ->
    lists:foreach(fun(Input) ->
        process(Input, Servers)
    end, Data).

file(Path, Servers) ->
    {ok, Device} = file:open(Path, read),
    read_lines(Device, Servers).

read_lines(Device, Servers) ->
    case file:read_line(Device) of
        {ok, Line} ->
            Input = check_lines(Line),
            process(Input, Servers),
            read_lines(Device, Servers);
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
