-module(part1).
-behaviour(gen_server).
-export([
    init/1,
    handle_call/3,
    handle_cast/2
]).

init(_) -> {ok, ok}.

handle_cast(_, _) -> {noreply, ok}.

handle_call(result, _, _) ->
    {
        reply,
        [
            { "Output", "part1 placeholder server" }
        ],
        ok
    };

handle_call(_, _, _) -> { reply, ok, ok }.