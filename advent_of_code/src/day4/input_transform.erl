-module(input_transform).
-behaviour(gen_server).

-export([
    init/1,
    handle_call/3,
    handle_cast/2
]).



init(_) ->
    {ok, 0}. % Lines processed to date, used to determine type of input.



handle_cast(reset, _) ->
    {noreply, 0}.



handle_call(Input, _, State) ->
    LineType = get_line_type(State),
    NewState = State + 1,
    case LineType of
        numbers ->
            Return = {numbers, process_numbers(Input)}, 
            {reply, Return, NewState};
        blank -> {noreply, NewState};
        {board, BoardIndex, RowIndex} ->
            Return = {board, BoardIndex, RowIndex, process_row(Input)},
            {reply, Return, NewState}
    end.



get_line_type(State) ->
    case State of
        0 -> numbers;
        _ ->
            RowsInBoard = 5, % key variable, maybe extract later.
            case (State - 1) rem (RowsInBoard + 1) of
                0 -> blank;
                _ ->
                    BoardIndex = (State - 2) div (RowsInBoard + 1),
                    RowIndex = (State - 2) rem (RowsInBoard + 1),
                    {board, BoardIndex, RowIndex}
            end
    end.



process_numbers(Input) ->
    process_numbers(Input, 0, []).

process_numbers([Head | Rest], CurrentValue, CurrentList) when Head == 44 ->
    process_numbers(Rest, 0, [CurrentValue | CurrentList]);

process_numbers([Head | Rest], CurrentValue, CurrentList) when Head >= 48, Head =< 57 ->
    NewValue = Head - 48,
    process_numbers(Rest, (10 * CurrentValue) + NewValue, CurrentList);

process_numbers([], CurrentValue, CurrentList) ->
    lists:reverse([CurrentValue | CurrentList]).



process_row(Input) ->
    process_row(Input, number, 0, []).

process_row([Head | Rest], number, CurrentValue, CurrentList) when Head == 32 ->
    process_row(Rest, space, 0, [CurrentValue | CurrentList]);

process_row([Head | Rest], number, CurrentValue, CurrentList) when Head >= 48, Head =< 57 ->
    NewValue = Head - 48,
    process_row(Rest, number, (10 * CurrentValue) + NewValue, CurrentList);

process_row([Head | Rest], space, _, CurrentList) when Head == 32 ->
    process_row(Rest, space, 0, CurrentList);

process_row([Head | Rest], space, _, CurrentList) when Head >= 48, Head =< 57 ->
    NewValue = Head - 48,
    process_row(Rest, number, NewValue, CurrentList);

process_row([], _, CurrentValue, CurrentList) ->
    lists:reverse([CurrentValue | CurrentList]).
