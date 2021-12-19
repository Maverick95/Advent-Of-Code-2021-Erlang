-module(day_2).
-export([
    
    start/0,
    
    forward_part_1/0,
    up_part_1/0,
    down_part_1/0,
    
    forward_part_2/0,
    up_part_2/0,
    down_part_2/0

]).



% Part 1 functions.



forward_part_1() ->
    receive
        {{Distance, Depth, Aim}, Value, PID} ->
            PID ! {Distance + Value, Depth, Aim}
    end,
    forward_part_1().

up_part_1() ->
    receive
        {{Distance, Depth, Aim}, Value, PID} ->
            PID ! {Distance, Depth - Value, Aim}
    end,
    up_part_1().

down_part_1() ->
    receive
        {{Distance, Depth, Aim}, Value, PID} ->
            PID ! {Distance, Depth + Value, Aim}
    end,
    down_part_1().



% Part 2 functions.



forward_part_2() ->
    receive
        {{Distance, Depth, Aim}, Value, PID} ->
            PID ! {Distance + Value, Depth + Aim * Value, Aim}
    end,
    forward_part_2().

up_part_2() ->
    receive
        {{Distance, Depth, Aim}, Value, PID} ->
            PID ! {Distance, Depth, Aim - Value}
    end,
    up_part_2().

down_part_2() ->
    receive
        {{Distance, Depth, Aim}, Value, PID} ->
            PID ! {Distance, Depth, Aim + Value}
    end,
    down_part_2().




start() ->
    register(forward, spawn(day_2, forward_part_1, [])),
    register(up, spawn(day_2, up_part_1, [])),
    register(down, spawn(day_2, down_part_1, [])),
    interface({0, 0, 0}),
    unregister(forward),
    unregister(up),
    unregister(down),
    0.



interface(Submarine) ->

    Result = io:fread("Please enter an instruction > ", "~a"),

    case Result of
        { ok, [Instruction | _] } when Instruction == forward; Instruction == up; Instruction == down ->
            
            Value = number(),
            Instruction ! {Submarine, Value, self()},
            receive SubmarineNew ->
                io:fwrite("Submarine data updated.~n"),
                interface(SubmarineNew)
            end;

        { ok, [result | _] } ->
            result(Submarine),
            interface(Submarine);

        { ok, [quit | _] } ->
            io:fwrite("Thanks for using!~n"),
            0;

        true ->
            io:fwrite("Please try again.~n"),
            interface(Submarine)

    end.

number() ->
    Result = io:fread("Please enter a number > ", "~u"),
    case Result of
        { ok, [Value | _] } ->
            Value;
        { error, _ } ->
            io:fwrite("Please try again.~n"),
            number()
    end.

result({Distance, Depth, Aim}) ->
    io:fwrite("Distance: ~10B~n", [Distance]),
    io:fwrite("Depth: ~10B~n", [Depth]),
    io:fwrite("Aim: ~10B~n", [Aim]).
