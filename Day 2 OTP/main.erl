-module(main).
-export([
    start/0,
    add/1,
    query/0,
    stop/0
]).

start() ->
    gen_event:start_link({local, aoc}),
    gen_event:add_handler(aoc, {data_handler, part1}, {part1, part1, part1_logger}),
    gen_event:add_handler(aoc, {data_handler, part2}, {part2, part2, part2_logger}),
    gen_server:start_link({local, aoc_input_transform}, input_transform, [], []),
    ok.

add(Data) ->
    Event = gen_server:call(aoc_input_transform, Data),
    case Event of
        error ->
            error;
        _ ->
            gen_event:notify(aoc, Event)
    end.

query() ->
    gen_event:notify(aoc, result).
    
stop() ->
    gen_event:stop(aoc),
    gen_server:stop(aoc_input_transform).