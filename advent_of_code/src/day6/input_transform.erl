-module(input_transform).
-behaviour(gen_server).

-export([
    init/1,
    handle_cast/2,
    handle_call/3
]).

init(_) ->
    {ok, ok}.

handle_call(Input, _, _) ->
    {reply, convert_to_number(Input), ok}.

handle_cast(reset, _) ->
    {noreply, ok}.

convert_to_number(Input) ->
    convert_to_number(Input, 0).

convert_to_number([], Current) ->
    Current;

convert_to_number([Head | Rest], Current) when Head >= 48, Head =< 57 ->
    Number = Head - 48,
    convert_to_number(Rest, (Current * 10) + Number).
