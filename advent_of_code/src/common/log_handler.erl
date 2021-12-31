-module(log_handler).
-behaviour(gen_event).

-export([
    init/1,
    handle_call/2,
    handle_event/2
]).

init(Name) ->
    {ok, Name}.

handle_call(_, Name) ->
    {ok, error, Name}.

handle_event({result, Server, Reply}, Name) ->
    io:format("--- Begin output for ~s ---~n", [Server]),
    print(Reply),
    io:format("--- End output for ~s ---~n", [Server]),
    {ok, Name}.

print([]) ->
    ok;

print([Next | Rest]) ->
    {Type, Value} = Next,
    io:format("Value of ~s: ", [Type]),
    io:format("= ~w~n", [Value]),
    print(Rest).
