-module(main).
-export([start/2, terminal/1, file/1,query/0]).

start(Server, Transform) ->
    supervisor:start_link({local, main_supervisor}, main_supervisor, {Server, Transform}).

terminal(Data) ->
    spawn(main_supervisor, start_input_terminal, [Data]).

file(File) ->
    spawn(main_supervisor, start_input_file, [File]).

query() ->
    spawn(main_supervisor, start_query, []).
