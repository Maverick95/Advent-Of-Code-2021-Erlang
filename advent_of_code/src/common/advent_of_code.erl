-module(advent_of_code).
-behaviour(application).
-export([
    start/2,
    terminal/1,
    file/1,
    process/1,
    query/0,
    reload/1,
    stop/1
]).

start(_, Args) ->
    supervisor:start_link(main_supervisor, Args).

terminal(Data) ->
    spawn(main_supervisor, start_input_terminal, [Data]).

file(File) ->
    spawn(main_supervisor, start_input_file, [File]).

process(Count) ->
    spawn(main_supervisor, start_process, [Count]).

query() ->
    spawn(main_supervisor, start_query, []).

stop(_) ->
    ok.





reload(Day) ->

    % Compile the 3 required code files into ebin.
    AsciiDay = day_to_ascii(Day),
    Directory = "../src/day" ++ AsciiDay,
    Files = ["part1", "part2", "input_transform"],
    lists:map(
        fun(File) ->
            FileAtom = list_to_atom(File),
            CompileResult = compile:file(Directory ++ "/" ++ File),
            case CompileResult of
                error -> error;
                _ ->
                    Loaded =
                    case code:is_loaded(FileAtom) of
                        {file, _} -> true;
                        _ -> false
                    end,
                    Deleted = code:delete(FileAtom),
                    Purge = Loaded and (not Deleted),
                    load(FileAtom, Purge)
            end
        end, Files).




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

