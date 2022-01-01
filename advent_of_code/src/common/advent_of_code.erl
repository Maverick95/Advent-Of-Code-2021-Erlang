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

reload(Day) ->
    spawn(main_supervisor, reload_servers, [Day]).

stop(_) ->
    ok.
