-module(input_transform).
-behaviour(gen_server).

-include_lib("eunit/include/eunit.hrl").

-export([
    init/1,
    handle_call/3,
    handle_cast/2
]).

% Server definitions

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

% get_line_type

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

get_line_type_test_() ->
    lists:map(
        fun({Input, Expected}) -> ?_assertEqual(Expected,
            get_line_type(Input) 
        ) end,
        [
            { 0, numbers },
            { 1, blank },
            { 2, { board, 0, 0 } },
            { 3, { board, 0, 1 } },
            { 4, { board, 0, 2 } },
            { 5, { board, 0, 3 } },
            { 6, { board, 0, 4 } },
            { 7, blank },
            { 8, { board, 1, 0 } },
            { 9, { board, 1, 1 } },
            { 10, { board, 1, 2 } },
            { 11, { board, 1, 3 } },
            { 12, { board, 1, 4 } },
            { 13, blank }
        ]).

% process_numbers

process_numbers(Input) ->
    process_numbers(Input, 0, []).

process_numbers([Head | Rest], CurrentValue, CurrentList) when Head == 44 ->
    process_numbers(Rest, 0, [CurrentValue | CurrentList]);

process_numbers([Head | Rest], CurrentValue, CurrentList) when Head >= 48, Head =< 57 ->
    NewValue = Head - 48,
    process_numbers(Rest, (10 * CurrentValue) + NewValue, CurrentList);

process_numbers([], CurrentValue, CurrentList) ->
    lists:reverse([CurrentValue | CurrentList]).

process_numbers_test_() ->
    lists:map(
        fun({Input, Expected}) -> ?_assertEqual(Expected,
            process_numbers(Input) 
        ) end,
        [
            {   "1,13,65,23,1",       [1,13,65,23,1]        },
            {   "119,05,7,8,1234",    [119,5,7,8,1234]      }
        ]).

% process_row

process_row(Input) ->
    process_row(Input, space, 0, []).

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

process_row([], space, _, CurrentList) ->
    lists:reverse(CurrentList);

process_row([], number, CurrentValue, CurrentList) ->
    lists:reverse([CurrentValue | CurrentList]).

process_row_test_() ->
    lists:map(
        fun({Input, Expected}) -> ?_assertEqual(Expected,
            process_row(Input) 
        ) end,
        [
            {   "1    13   5 23    4 5",    [1,13,5,23,4,5]     },
            {   "789 3  9  4   1432 ",      [789,3,9,4,1432]    },
            {   "   78  4 64  5  143 7",    [78,4,64,5,143,7]   }
        ]).
