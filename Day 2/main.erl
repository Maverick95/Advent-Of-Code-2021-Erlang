-module(main).
-export([
    start/1,
    stop/0
]).



start(Mod) ->
    register(forward, spawn(Mod, forward, [])),
    register(up, spawn(Mod, up, [])),
    register(down, spawn(Mod, down, [])),
    interface({0, 0, 0}).

stop() ->
    unregister(forward),
    unregister(up),
    unregister(down).



interface(Data) ->

    Result = io:fread("Please enter an instruction > ", "~a"),

    case Result of
        { ok, [Instruction | _] } when Instruction == forward; Instruction == up; Instruction == down ->
            
            Value = number(),
            Instruction ! {Data, Value, self()},
            receive DataNew ->
                io:fwrite("Submarine data updated.~n"),
                interface(DataNew)
            end;

        { ok, [result | _] } ->
            result(Data),
            interface(Data);

        { ok, [quit | _] } ->
            io:fwrite("Thanks for using!~n"),
            0;

        _Else ->
            io:fwrite("Please try again.~n"),
            interface(Data)

    end.

number() ->
    Result = io:fread("Please enter a number > ", "~u"),
    case Result of
        { ok, [Value | _] } ->
            Value;
        _Else ->
            io:fwrite("Please try again.~n"),
            number()
    end.

result({Distance, Depth, Aim}) ->
    io:fwrite("Distance: ~10B~n", [Distance]),
    io:fwrite("Depth: ~10B~n", [Depth]),
    io:fwrite("Aim: ~10B~n", [Aim]).
