-module(main).
-export([
    start/2,
    add/1,
    import/1,
    query/0,
    stop/0
]).

start(Server, Transform) ->
    
    gen_server:start_link({local, aoc_input_transform}, Transform, [], []),

    gen_event:start_link({local, aoc_manager}),
    gen_event:start_link({local, aoc_input_manager}),

    gen_event:add_handler(aoc_manager, {data_handler, handler}, {Server, handler_server, handler_logger}),
    gen_event:add_handler(aoc_input_manager, input_handler, {aoc_input_transform, aoc_manager}),

    ok.

add(Data) ->
    lists:foreach(fun(X) -> gen_event:notify(aoc_input_manager, X) end, Data).

import(File) ->
    spawn(file_import, import, [File, aoc_input_manager]).

query() ->
    gen_event:notify(aoc_manager, result).
    
stop() ->
    gen_event:stop(aoc_manager),
    gen_event:stop(aoc_input_manager),
    gen_server:stop(aoc_input_transform).