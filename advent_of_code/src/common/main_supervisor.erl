-module(main_supervisor).
-behaviour(supervisor).
-export([
    start_logger/0,
    start_input_terminal/1,
    start_input_file/1,
    start_input_file/2,
    start_process/1,
    start_query/0,
    init/1,
    reset_servers/0,
    reload_servers/1
]).

start_logger() ->
    Handler = gen_event:start_link({local, aoc_logger_manager}),
    gen_event:add_handler(aoc_logger_manager, log_handler, aoc_logger),
    Handler.

start_input_terminal(Data) ->
    import:terminal(Data,
        [
            aoc_input_transform,
            aoc_data_server_0,
            aoc_data_server_1
        ]).

start_input_file(File) ->
    import:file(File,
        [
            aoc_input_transform,
            aoc_data_server_0,
            aoc_data_server_1
        ]).

start_input_file(File, Separator) ->
    import:file(File, Separator,
        [
            aoc_input_transform,
            aoc_data_server_0,
            aoc_data_server_1
        ]).

start_process(Count) ->
    start_process(0, Count).
    
start_process(Index, Count) when Index == Count ->
    ok;

start_process(Index, Count) ->
    parts:process(
        [
            aoc_data_server_0,
            aoc_data_server_1
        ], process),
    start_process(Index + 1, Count).

start_query() ->
    parts:result(
        [
            aoc_data_server_0,
            aoc_data_server_1
        ],
        aoc_logger_manager).

init(Servers) ->
    reload_servers(fallback),
    {
        ok,
        {
            #{},
            [
                #{
                    id => sup_logger,
                    start => {
                        main_supervisor,
                        start_logger,
                        []
                    }
                },
                #{
                    id => sup_day_supervisor,
                    start => {
                        supervisor,
                        start_link,
                        [
                            {local, aoc_day_supervisor},
                            day_supervisor,
                            {
                                Servers,
                                aoc_input_transform,
                                "aoc_data_server"
                            }
                        ]
                    }
                }
            ]
        }
    }.

reset_servers() ->
    parts:process(
        [
            aoc_input_transform,
            aoc_data_server_0,
            aoc_data_server_1
        ], reset).

reload_servers(Args) ->
    Files = [
        { "part1", aoc_data_server_0 },
        { "part2", aoc_data_server_1 },
        { "input_transform", aoc_input_transform } ],

    _Results = lists:map(
        fun({File, _}) ->
            case Args of
                fallback ->
                    load_fallback_file(File);
                {target, Day} ->
                    load_target_file(File, Day)
            end
        end, Files),

    _Resets = lists:foreach(fun({_, Server}) -> gen_server:cast(Server, reset) end, Files),
    ok.

load_target_file(File, Day) ->
    FileTarget = get_target_file(File, Day),
    case compile:file(FileTarget) of
        error ->
            load_fallback_file(File);
        _ ->
            load(File)
    end.

load_fallback_file(File) ->
    FileFallback = get_fallback_file(File),
    compile:file(FileFallback),
    load(File).

get_target_file(File, Day) ->
    AsciiDay = day_to_ascii(Day),
    "../src/day" ++ AsciiDay ++ "/" ++ File.

get_fallback_file(File) ->
    "../src/template" ++ "/" ++ File.

load(File) ->
    FileAtom = list_to_atom(File),
    code:purge(FileAtom),
    code:delete(FileAtom),
    {Result, _} = code:load_file(FileAtom),
    Result.
    
day_to_ascii(Day) when Day > 0 ->
    day_to_ascii(Day, []).

day_to_ascii(Day, Current) when Day == 0 ->
    Current;

day_to_ascii(Day, Current) ->
    Remainder = Day rem 10,
    Head = 48 + Remainder,
    Next = (Day - Remainder) div 10,
    day_to_ascii(Next, [Head | Current]).
