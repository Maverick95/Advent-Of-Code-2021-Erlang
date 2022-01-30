-module(input_transform).
-behaviour(gen_server).

-export([
    init/1,
    handle_call/3,
    handle_cast/2
    ]).

init(_) ->
    {ok, ok}.

handle_call(Input, _, _) ->
    {reply, create_coords(Input), ok}.

handle_cast(reset, _) ->
    {noreply, ok}.

create_coords(Input) ->
    Split = string:split(Input, " -> ", all),
    2 = length(Split),
    Values = lists:map(fun (X) -> create_coord(X) end, Split),
    [Start, End] = Values,
    {Start, End}.

create_coord(Input) ->
    Split = string:split(Input, ",", all),
    2 = length(Split),
    Values = lists:map(fun (X) -> to_integer(X) end, Split),
    [X, Y] = Values,
    {X, Y}.

to_integer(Input) ->
    {Result, []} = string:to_integer(Input),
    if Result >= 0 ->
        Result
    end.
