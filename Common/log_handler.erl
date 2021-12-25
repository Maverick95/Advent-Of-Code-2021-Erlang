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

handle_event(Data, Name) ->
    io:format("--- Begin output for ~s ---~n", [Name]),
    print(Data, Name),
    io:format("--- End output for ~s ---~n", [Name]),
    {ok, Name}.

print([Next | Rest], Name) ->
    {Type, Value} = Next,
    io:format("Value of ~s: ", [Type]),
    io:format("= ~w~n", [Value]),
    print(Rest, Name);

print([], _) ->
    ok.
