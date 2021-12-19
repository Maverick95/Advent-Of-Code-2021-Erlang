-module(part1).
-export([
    forward/0,
    up/0,
    down/0
]).



forward() ->
    receive
        {{Distance, Depth, Aim}, Value, PID} ->
            PID ! {Distance + Value, Depth, Aim}
    end,
    forward().

up() ->
    receive
        {{Distance, Depth, Aim}, Value, PID} ->
            PID ! {Distance, Depth - Value, Aim}
    end,
    up().

down() ->
    receive
        {{Distance, Depth, Aim}, Value, PID} ->
            PID ! {Distance, Depth + Value, Aim}
    end,
    down().