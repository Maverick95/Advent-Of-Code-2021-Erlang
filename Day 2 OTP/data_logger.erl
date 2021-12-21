-module(data_logger).
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
    print(Data, Name),
    {ok, Name}.

print([Next | Rest], Name) ->
    {Type, Value} = Next,
    io:format("Value of ~s~n", [Type]),
    io:format("= ~B~n", [Value]),
    print(Rest, Name);

print([], Name) ->
    io:format("---~s~n", [Name]).
