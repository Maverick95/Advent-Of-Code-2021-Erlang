-module(parts).

-export([
    result/2,
    process/2
]).

result(Servers, Logger) ->
    lists:foreach(fun(Server) ->
        Result = gen_server:call(Server, result),
        gen_event:notify(Logger, {result, Server, Result})
    end, Servers).

process(Servers, Request) when Request == process; Request == reset ->
    lists:foreach(fun(Server) ->
        gen_server:cast(Server, Request)
    end, Servers).
