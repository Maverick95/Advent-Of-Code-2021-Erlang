-module(input_transform).
-behaviour(gen_server).

-export([
    init/1,
    handle_call/3,
    handle_cast/2
    ]).



init(_) ->
    {ok, [{"forward", forward}, {"up", up}, {"down", down}]}.



handle_call(Input, _, State) ->
    Result = transform(Input, State),
    case Result of
        {ok, Transform} -> {reply, Transform, State};
        error -> {reply, error, State}
    end.



handle_cast(_, State) ->
    {noreply, State}.



transform(Input, instruction) ->
    transform(Input, instruction, []);

transform(Value, value) ->
    transform(Value, value, 0);

transform(Input, State) ->
    TransformInstruction = transform(Input, instruction),
    case TransformInstruction of
        error -> error;
        _Else ->
            {StringInstruction, StringValue} = TransformInstruction,
            Instruction = find(StringInstruction, State),
            Value = transform(StringValue, value),
            if
                Instruction /= error, Value /= error ->
                    {ok, {Instruction, Value}};
                true ->
                    error
            end
    end.



transform([Head | Rest], instruction, Current) ->
    case Head of
        32 ->
            {Current, Rest};
        _ ->
            transform(Rest, instruction, Current++[Head])
    end;

transform([], instruction, _) ->
    error;

transform([Head | Rest], value, Current) ->
    Number =
        case Head of
            Digit when Digit >= 48, Digit =< 57 ->
                Digit - 48;
            _ ->
                error
        end,
    case Number of
        error ->
            error;
        _ ->
            transform(Rest, value, Number + Current * 10)
    end;

transform([], value, Current) ->
    Current.



find(Lookup, [Head | Rest]) ->
    {StringInstruction, Instruction} = Head,
    case StringInstruction of
        Lookup ->
            Instruction;
        _ ->
            find(Lookup, Rest)
    end;

find(_, []) ->
    error.
