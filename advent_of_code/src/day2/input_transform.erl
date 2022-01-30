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
    Transform = transform(Input, State),
    {reply, Transform, State}.

handle_cast(reset, _) ->
    {noreply, [{"forward", forward}, {"up", up}, {"down", down}]}.

transform(Input, instruction) ->
    transform(Input, instruction, []);

transform(Value, value) ->
    transform(Value, value, 0);

transform(Input, State) ->
    {StringInstruction, StringValue} = transform(Input, instruction),
    Instruction = find(StringInstruction, State),
    Value = transform(StringValue, value),
    {Instruction, Value}.

transform([Head | Rest], instruction, Current) ->
    case Head of
        32 ->
            {Current, Rest};
        _ ->
            transform(Rest, instruction, Current++[Head])
    end;

transform([Head | Rest], value, Current) ->
    Number =
        case Head of
            Digit when Digit >= 48, Digit =< 57 ->
                Digit - 48
        end,
    transform(Rest, value, Number + Current * 10);

transform([], value, Current) ->
    Current.

find(Lookup, [Head | Rest]) ->
    {StringInstruction, Instruction} = Head,
    case StringInstruction of
        Lookup ->
            Instruction;
        _ ->
            find(Lookup, Rest)
    end.
