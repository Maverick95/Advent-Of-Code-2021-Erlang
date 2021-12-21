-module(main).
-export([
    start/0,
    add/1,
    stop/0
]).

start() ->
    gen_event:start_link({local, aoc}),
    gen_event:add_handler(aoc, {data_handler, part1}, {part1, part1, part1_logger}),
    gen_event:add_handler(aoc, {data_handler, part2}, {part2, part2, part2_logger}),
    ok.

add(Data) ->
    gen_event:notify(aoc, Data).
    
stop() ->
    gen_event:stop(aoc).