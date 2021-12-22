-module(main).
-export([
    start/2,
    add/1,
    query/0,
    stop/0
]).

start(Server, Transform) ->
    gen_event:start_link({local, aoc_manager}),
    gen_event:add_handler(aoc_manager, {data_handler, handler}, {Server, handler_server, handler_logger}),
    gen_server:start_link({local, aoc_input_transform}, Transform, [], []),
    ok.

add(Data) ->
    Event = gen_server:call(aoc_input_transform, Data),
    case Event of
        error ->
            error;
        _ ->
            gen_event:notify(aoc_manager, Event)
    end.

query() ->
    gen_event:notify(aoc_manager, result).
    
stop() ->
    gen_event:stop(aoc_manager),
    gen_server:stop(aoc_input_transform).