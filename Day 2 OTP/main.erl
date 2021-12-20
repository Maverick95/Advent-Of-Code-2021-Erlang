-module(main).
-export([
    start/1,
    stop/0
]).

start(Mod) ->
    gen_server:start_link({local, server}, Mod, [], []).

stop() ->
    gen_server:cast(server, quit).