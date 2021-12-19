-module(part2).
-export([
    forward/0,
    up/0,
    down/0
]).



forward() ->
    receive
        {{Distance, Depth, Aim}, Value, PID} ->
            PID ! {Distance + Value, Depth + Aim * Value, Aim}
    end,
    forward().

up() ->
    receive
        {{Distance, Depth, Aim}, Value, PID} ->
            PID ! {Distance, Depth, Aim - Value}
    end,
    up().

down() ->
    receive
        {{Distance, Depth, Aim}, Value, PID} ->
            PID ! {Distance, Depth, Aim + Value}
    end,
    down().