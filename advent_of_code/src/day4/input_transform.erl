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
        blank -> {reply, blank, NewState};
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

% Main test function for input_transform.

input_transform_test() ->

    % SETUP

    TryResult = gen_server:start_link({local, test_input_transform}, input_transform, [], []),
    Result = case TryResult of
        {error, {already_started, _}} ->
            gen_server:stop(test_input_transform),
            gen_server:start_link({local, test_input_transform}, input_transform, [], []);
        _ -> TryResult
    end,
    {ok, _} = Result,
    
    % ARRANGE

    Data = [
        {
            "7,4,9,5,11,17,23,2,0,14,21,24,10,16,13,6,15,25,12,22,18,20,8,19,3,26,1",
            {numbers, [7,4,9,5,11,17,23,2,0,14,21,24,10,16,13,6,15,25,12,22,18,20,8,19,3,26,1]}
        },
        {
            "",
            blank
        },
        {
            "22 13 17 11  0",
            {board, 0, 0, [22,13,17,11,0]}
        },
        {
            " 8  2 23  4 24",
            {board, 0, 1, [8,2,23,4,24]}
        },
        {
            "21  9 14 16  7",
            {board, 0, 2, [21,9,14,16,7]}
        },
        {
            " 6 10  3 18  5",
            {board, 0, 3, [6,10,3,18,5]}
        },
        {
            " 1 12 20 15 19",
            {board, 0, 4, [1,12,20,15,19]}
        },
        {
            "",
            blank
        },
        {
            " 3 15  0  2 22",
            {board, 1, 0, [3,15,0,2,22]}
        },
        {
            " 9 18 13 17  5",
            {board, 1, 1, [9,18,13,17,5]}
        },
        {
            "19  8  7 25 23",
            {board, 1, 2, [19,8,7,25,23]}
        },
        {
            "20 11 10 24  4",
            {board, 1, 3, [20,11,10,24,4]}
        },
        {
            "14 21 16 12  6",
            {board, 1, 4, [14,21,16,12,6]}
        },
        {
            "",
            blank
        },
        {
            "14 21 17 24  4",
            {board, 2, 0, [14,21,17,24,4]}
        },
        {
            "10 16 15  9 19",
            {board, 2, 1, [10,16,15,9,19]}
        },
        {
            "18  8 23 26 20",
            {board, 2, 2, [18,8,23,26,20]}
        },
        {
            "22 11 13  6  5",
            {board, 2, 3, [22,11,13,6,5]}
        },
        {
            " 2  0 12  3  7",
            {board, 2, 4, [2,0,12,3,7]}
        }
    ],
    
    % ACT and ASSERT

    lists:foreach(
        fun ({Input, Expected}) ->
            Actual = gen_server:call(test_input_transform, Input),
            ?assertEqual(Expected, Actual)
        end, Data),

    % TEARDOWN

    gen_server:stop(test_input_transform).
