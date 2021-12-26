-module(main).
-export([start/2, terminal/1, file/1, process/1, query/0]).

start(Servers, Transform) ->
    supervisor:start_link({local, main_supervisor}, main_supervisor, {Servers, Transform}).

terminal(Data) ->
    spawn(main_supervisor, start_input_terminal, [Data]).

file(File) ->
    spawn(main_supervisor, start_input_file, [File]).

process(Count) ->
    spawn(main_supervisor, start_process, [Count]).

query() ->
    spawn(main_supervisor, start_query, []).
