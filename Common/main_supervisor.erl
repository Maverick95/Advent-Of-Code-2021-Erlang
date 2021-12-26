-module(main_supervisor).
-behaviour(supervisor).
-export([
    start_logger/0,
    start_data_handler/1,
    start_input_handler/0,
    start_input_terminal/1,
    start_input_file/1,
    start_query/0,
    init/1
]).



% Reference names to supervisors / children are as follows -

% Logger - sup_logger
% Manager - aoc_logger_manager

% Data handler - sup_data_handler
% Manager - aoc_manager

% Input handler - sup_input_handler
% Manager - aoc_input_manager



start_logger() ->
    Handler = gen_event:start_link({local, aoc_logger_manager}),
    gen_event:add_handler(aoc_logger_manager, log_handler, aoc_logger),
    Handler.



start_data_handler(ServerDetails) ->
    Handler = gen_event:start_link({local, aoc_manager}), 
    lists:foreach(fun(X) ->
       {_, Id, _} = X,
       gen_event:add_handler(aoc_manager, data_handler, {Id, aoc_logger_manager}) end,
       ServerDetails),
    Handler.



start_input_handler() ->
    Handler = gen_event:start_link({local, aoc_input_manager}),
    gen_event:add_handler(aoc_input_manager, input_handler, {aoc_input_transform, aoc_manager}),
    Handler.



start_input_terminal(Data) ->
    import:terminal(Data, aoc_input_manager).



start_input_file(File) ->
    import:file(File, aoc_input_manager).



start_query() ->
    gen_event:notify(aoc_manager, result).



init({Servers, Transform}) ->

    ServerDetails = server_details(Servers),
    ServerConfigs = server_configs(ServerDetails),

    BaseConfigs = [
                #{
                    id => sup_logger,
                    start => {main_supervisor, start_logger, []}
                },
                #{
                    id => sup_data_handler,
                    start => {main_supervisor, start_data_handler, [ServerDetails]}
                },
                #{
                    id => sup_input_server,
                    start => {gen_server, start_link, [{local, aoc_input_transform}, Transform, [], []]}
                },
                #{
                    id => sup_input_handler,
                    start => {main_supervisor, start_input_handler, []}
                }
            ],

    {
        ok,
        {
            #{},
            ServerConfigs ++ BaseConfigs
        }
    }.



server_details(Servers) ->
    server_details(Servers, [], 0).

server_details([], Current, _) -> Current;

server_details([Module | Rest], Current, Index) ->
    Suffix = integer_to_list(Index),
    SupId = list_to_atom("sup_data_server_" ++ Suffix),
    Id = list_to_atom("aoc_data_server_" ++ Suffix),
    Next = {SupId, Id, Module},
    server_details(Rest, Current ++ [Next], Index + 1).



server_configs(Details) ->
    lists:map(
        fun({SupId, Id, Module}) ->
        #{
            id => SupId,
            start => {gen_server, start_link, [{local, Id}, Module, [], []]}
        } end, Details).
