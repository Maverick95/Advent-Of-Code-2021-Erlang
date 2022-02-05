-module(import).
-export([
    terminal/2,
    file/2,
    file/3
]).

terminal(Data, Servers) ->
    lists:foreach(fun(Input) ->
        process(Input, Servers)
    end, Data).

file(Path, Servers) ->
    file(Path, [13, 10], Servers).

file(Path, Separator, Servers) ->
    {ok, Device} = file:open(Path, read),
    read(Device, [], Separator, 1, Servers).

read(Device, Input, Separator, SeparatorIndex, Servers) when SeparatorIndex > length(Separator) ->
    process(Input, Servers),
    read(Device, [], Separator, 1, Servers);

read(Device, Input, Separator, SeparatorIndex, Servers) ->
    case file:read(Device, 1) of
        {ok, [Next]} ->
            SeparatorNext = lists:nth(SeparatorIndex, Separator),
            if
                Next == SeparatorNext ->
                    read(Device, Input, Separator, SeparatorIndex + 1, Servers);
                true ->
                    read(Device, Input ++ [Next], Separator, 1, Servers)
            end;
        eof ->
            process(Input, Servers),
            ok = file:close(Device)
    end.

process(Input, Servers) ->
    [Transform | Parts] = Servers,
    Result = gen_server:call(Transform, Input),
    lists:foreach(fun(Part) ->
        gen_server:cast(Part, Result)
    end, Parts).
