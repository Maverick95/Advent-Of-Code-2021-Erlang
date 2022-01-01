-module(main_supervisor).
-behaviour(supervisor).
-export([
    start_logger/0,
    start_data_handler/1,
    start_input_handler/0,
    start_input_terminal/1,
    start_input_file/1,
    start_process/1,
    start_query/0,
    init/1,
    reload_servers/1,
    reset_servers/0
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



start_process(Count) ->
    start_process(0, Count).
    
start_process(Index, Count) when Index >= Count ->
    ok;

start_process(Index, Count) ->
    gen_event:notify(aoc_manager, process),
    start_process(Index + 1, Count).



start_query() ->
    gen_event:notify(aoc_manager, result).



init({Servers, Transform}) ->

    ServerDetails = server_details(Servers),

    LoggerConfigs = [
        #{
            id => sup_logger,
            start => {main_supervisor, start_logger, []}
        }
    ],

    ServerConfigs = server_configs(ServerDetails),

    BaseConfigs = [     
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
            LoggerConfigs ++ ServerConfigs ++ BaseConfigs
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



reload_servers(Day) ->

    Files = [
        { "part1", aoc_data_server_0 },
        { "part2", aoc_data_server_1 },
        { "input_transform", aoc_input_transform } ],

    _Results = lists:map(fun({File, _}) -> full_load(File, Day) end, Files),
    _Resets = lists:foreach(fun({_, Server}) -> spawn(gen_server, cast, [Server, reset]) end, Files),
    ok.



reset_servers() ->

    Files = [
        { "part1", aoc_data_server_0 },
        { "part2", aoc_data_server_1 },
        { "input_transform", aoc_input_transform } ],

    _Resets = lists:foreach(fun({_, Server}) -> spawn(gen_server, cast, [Server, reset]) end, Files),
    ok.



full_load(File, Day) ->
    AsciiDay = day_to_ascii(Day),
    Directory = "../src/day" ++ AsciiDay,
    FileAtom = list_to_atom(File),
    CompileResult = compile:file(Directory ++ "/" ++ File),
    if CompileResult /= error ->
        Loaded =
            case code:is_loaded(FileAtom) of
                {file, _} -> true;
                _ -> false
            end,
        Deleted = code:delete(FileAtom),
        Purge = Loaded and (not Deleted),
        {File, load(FileAtom, Purge)};
    true ->
        {File, error}
    end.
    


day_to_ascii(Day) when Day > 0 ->
    day_to_ascii(Day, []).

day_to_ascii(Day, Current) when Day == 0 ->
    Current;

day_to_ascii(Day, Current) ->
    Remainder = Day rem 10,
    Head = 48 + Remainder,
    Next = (Day - Remainder) div 10,
    day_to_ascii(Next, [Head | Current]).



load(FileAtom, false) ->
    straight_load(FileAtom);

load(FileAtom, true) ->
    code:purge(FileAtom),
    code:delete(FileAtom),
    straight_load(FileAtom).



straight_load(FileAtom) ->
    {Result, _} = code:load_file(FileAtom),
    Result.
