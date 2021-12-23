-module(input_transform).
-behaviour(gen_server).

-export([
    init/1,
    handle_call/3,
    handle_cast/2
    ]).



init(_) ->
    {ok, 0}.



handle_call(Input, _, _) ->
    {reply, create_coords(Input), 0}.



handle_cast(_, _) ->
    {noreply, 0}.



create_coords(Input) ->
    Split = string:split(Input, " -> ", all),
    case length(Split) of
        2 ->
            Values = lists:map(Split, fun (X) -> create_coord(X) end),
            Valid = lists:all(Values, fun (X) -> X /= error end),
            case Valid of
                true ->
                    [Start, End] = Values,
                    {Start, End};
                false ->
                    error
            end;    
        _ ->
            error
    end.



create_coord(Input) ->
    Split = string:split(Input, ",", all),
    case length(Split) of
        2 ->
            Values = lists:map(Split, fun (X) -> to_integer(X) end),
            Valid = lists:all(Values, fun (X) -> X /= error end), 
            case Valid of
                true ->
                    [X, Y] = Values,
                    {X, Y};
                false ->
                    error
            end;
        _ ->
            error
    end.



to_integer(Input) ->
    Result = string:to_integer(Input),
    case Result of
        {error, _} ->
            error;
        {Value, Rest} ->
            if
                Value >= 0, Rest == "" ->
                    Value;
                true ->
                    error
            end
    end.
