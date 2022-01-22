-module(part2).
-behaviour(gen_server).

-include_lib("eunit/include/eunit.hrl").

-export([
    init/1,
    handle_cast/2,
    handle_call/3
]).

% Initialize

init(_) ->
    {
        ok,
        #{
            numbers => [],
            numbers_processed => [],
            boards => #{},
            boards_with_completes => [],
            lookups => #{}
        }
    }.

% Reset

handle_cast(reset, _) ->
    {
        noreply,
        #{
            numbers => [],
            numbers_processed => [],
            boards => #{},
            boards_with_completes => [],
            lookups => #{}
        }
    };

% Handle numbers events

handle_cast({numbers, NumbersToAdd}, State) ->
    #{numbers := Numbers} = State,
    {
        noreply,
        State#{
            numbers := Numbers ++ NumbersToAdd
        }
    };

% Handle blank event (do nothing)

handle_cast(blank, State) ->
    { noreply, State };

% Handle board events

handle_cast({board, BoardIndex, RowIndex, ColumnValues}, State) ->
    #{boards := Boards, lookups := Lookups} = State,
    Board = case Boards of
        #{BoardIndex := BoardCurrent} -> BoardCurrent;
        _ -> #{
            total => 0,
            rows => #{0 => {0, 0}, 1 => {0, 0}, 2 => {0, 0}, 3 => {0, 0}, 4 => {0, 0}},
            columns => #{0 => {0, 0}, 1 => {0, 0}, 2 => {0, 0}, 3 => {0, 0}, 4 => {0, 0}}
        }
    end,
    #{total := Total, rows := Rows, columns:= Columns} = Board,
    { TotalNew, RowTotalNew, ColumnsNew } = update_board_totals(Total, Columns, ColumnValues),
    BoardNew = Board#{
        total := TotalNew,
        rows := Rows#{RowIndex := RowTotalNew},
        columns := ColumnsNew
    },
    BoardsNew = Boards#{BoardIndex => BoardNew},
    LookupsNew = update_lookups(Lookups, BoardIndex, RowIndex, ColumnValues),
    {
        noreply,
        State#{
            boards := BoardsNew,
            lookups := LookupsNew
        }
    };

% Handle process events

handle_cast(process, State) ->
    #{
        numbers := Numbers,
        numbers_processed := NumbersProcessed,
        boards := Boards,
        boards_with_completes := BoardsWithCompletes,
        lookups := Lookups
    } = State,

    StateNew = case Numbers of
        [NumbersNext | NumbersRest] ->
            NotProcessed = lists:all(fun(X) -> X /= NumbersNext end, NumbersProcessed),
            if
                NotProcessed ->
                    {BoardsNew, BoardsWithCompletesNew} = lookup_and_subtract(NumbersNext, Lookups, Boards, BoardsWithCompletes),
                    State#{
                        boards := BoardsNew,
                        boards_with_completes := BoardsWithCompletesNew,
                        numbers := NumbersRest,
                        numbers_processed := [NumbersNext | NumbersProcessed]
                        };
                true ->
                    State
            end;
        _ ->
            State
        end,
    {
        noreply,
        StateNew
    }.

% Helper function to update board given row input as Columns list

update_board_totals(Total, Columns, ColumnValues) ->
    RowTotal = {0, 0},
    Index = 0,
    update_board_totals(Total, RowTotal, Columns, Index, ColumnValues).

update_board_totals(Total, {RowTotalCount, RowTotalSum}, Columns, Index, [ColumnValuesNext | ColumnValuesRest]) ->
    #{Index := {ColumnTotalCount, ColumnTotalSum}} = Columns,
    update_board_totals(
        Total + ColumnValuesNext,
        {RowTotalCount + 1, RowTotalSum + ColumnValuesNext},
        Columns#{Index := {ColumnTotalCount + 1, ColumnTotalSum + ColumnValuesNext}},
        Index + 1,
        ColumnValuesRest);

update_board_totals(Total, RowTotal, Columns, _, []) ->
    { Total, RowTotal, Columns }.

update_board_totals_test_() ->
    lists:map(
        fun({Total, Columns, ColumnValues, Expected}) -> ?_assertEqual(Expected,
            update_board_totals(Total, Columns, ColumnValues) 
        ) end,
        [
            {
            % Inputs
            100,    #{0 => {0, 0}, 1 => {1, 1}, 2 => {1, 2}, 3 => {1, 3}, 4 => {1, 4}},     [10,15,20,25,30],
            % Outputs
            {
                200,
                {5, 100},
                #{0 => {1, 10}, 1 => {2, 16}, 2 => {2, 22}, 3 => {2, 28}, 4 => {2, 34}}
            }
            }
        ]).

% Helper function to update lookups during board input

update_lookups(Lookups, BoardIndex, RowIndex, ColumnValues) ->
    update_lookups(Lookups, BoardIndex, RowIndex, 0, ColumnValues).

update_lookups(Lookups, BoardIndex, RowIndex, ColumnIndex, [ColumnValueNext | ColumnValueRest]) ->
    Lookup = case Lookups of
        #{ColumnValueNext := LookupCurrent} -> LookupCurrent;
        _ -> []
    end,
    LookupNew = [ {BoardIndex, RowIndex, ColumnIndex} | Lookup],
    update_lookups(Lookups#{ColumnValueNext => LookupNew}, BoardIndex, RowIndex, ColumnIndex + 1, ColumnValueRest);

update_lookups(Lookups, _, _, _, []) ->
    Lookups.

update_lookups_test_() ->
    lists:map(
        fun({Lookups, BoardIndex, RowIndex, ColumnValues, Expected}) -> ?_assertEqual(Expected,
            update_lookups(Lookups, BoardIndex, RowIndex, ColumnValues) 
        ) end,
        [
            {
            % Inputs
            #{
                1 => [ {0, 0, 0} ],
                2 => [ {0, 0, 1} ],
                3 => [ {0, 0, 2} , {0, 0, 3} , {0, 0, 4} ]
            },
            0, 1,
            [5,5,3,2,1],
            % Outputs
            #{
                1 => [ {0, 1, 4} , {0, 0, 0} ],
                2 => [ {0, 1, 3} , {0, 0, 1} ],
                3 => [ {0, 1, 2} , {0, 0, 2} , {0, 0, 3} , {0, 0, 4} ],
                5 => [ {0, 1, 1} , {0, 1, 0} ]
            }
            }
        ]).

% Process function.

lookup_and_subtract(Number, Lookups, Boards, BoardsWithCompletes) ->
    case Lookups of
        #{Number := Squares} ->
            subtract(Number, Squares, Boards, BoardsWithCompletes);
        _ ->
            Boards
    end.

subtract(_, [], Boards, BoardsWithCompletes) ->
    {Boards, BoardsWithCompletes};

subtract(Number, [{BoardIndex, RowIndex, ColumnIndex} | SquaresRest], Boards, BoardsWithCompletes) ->
    
    #{BoardIndex := Board} = Boards,
    #{total := Total, rows := Rows, columns:= Columns} = Board,
    #{RowIndex := {RowTotalCount, RowTotalSum}} = Rows,
    #{ColumnIndex := {ColumnTotalCount, ColumnTotalSum}} = Columns,

    BoardsWithCompletesNew =
        if
            (RowTotalCount == 1) or (ColumnTotalCount == 1) ->
                case lists:all(fun(X) -> X /= BoardIndex end, BoardsWithCompletes) of
                    true -> [BoardIndex | BoardsWithCompletes];
                    false -> BoardsWithCompletes
                end;
            true ->
                BoardsWithCompletes
        end, 

    BoardNew = Board#{
        total := Total - Number,
        rows := Rows#{RowIndex := {RowTotalCount - 1, RowTotalSum - Number}},
        columns := Columns#{ColumnIndex := {ColumnTotalCount - 1, ColumnTotalSum - Number}}
    },

    BoardsNew = Boards#{
        BoardIndex := BoardNew
    },

    subtract(Number, SquaresRest, BoardsNew, BoardsWithCompletesNew).

% Query

handle_call(result, _, State) ->
    #{
        numbers := Numbers,
        numbers_processed := NumbersProcessed,
        boards := Boards,
        boards_with_completes := BoardsWithCompletes
    } = State,
    {
        reply,
        [
            {"Numbers", Numbers},
            {"Numbers Processed", NumbersProcessed},
            {"Boards", Boards},
            {"Boards with Completes", BoardsWithCompletes},
            {"Boards Length", maps:size(Boards)},
            {"Boards with Completes Length", length(BoardsWithCompletes)}
        ],
        State
    }.

% Main test function for part1.

part1_test() ->

    % SETUP

    TryResult = gen_server:start_link({local, test_part1}, part1, [], []),
    Result = case TryResult of
        {error, {already_started, _}} ->
            gen_server:stop(test_part1),
            gen_server:start_link({local, test_part1}, part1, [], []);
        _ -> TryResult
    end,
    {ok, _} = Result,

    % ARRANGE

    Data = [
        {numbers, [7,4,9,5,11,17,23,2,0,14,21,24,10,16,13,6,15,25,12,22,18,20,8,19,3,26,1]},
        blank,
        {board, 0, 0, [22, 13, 17, 11, 0]},
        {board, 0, 1, [8,  2,  23, 4,  24]},
        {board, 0, 2, [21, 9,  14, 16, 7]},
        {board, 0, 3, [6,  10, 3,  18, 5]},
        {board, 0, 4, [1,  12, 20, 15, 19]},
        blank,
        {board, 1, 0, [3,  15, 0,  2,  22]},
        {board, 1, 1, [9,  18, 13, 17, 5]},
        {board, 1, 2, [19, 8,  7,  25, 23]},
        {board, 1, 3, [20, 11, 10, 24, 4]},
        {board, 1, 4, [14, 21, 16, 12, 6]},
        blank,
        {board, 2, 0, [14, 21, 17, 24, 4]},
        {board, 2, 1, [10, 16, 15, 9,  19]},
        {board, 2, 2, [18, 8,  23, 26, 20]},
        {board, 2, 3, [22, 11, 13, 6,  5]},
        {board, 2, 4, [2,  0,  12, 3,  7]}
    ],

    ExpectedPart1 = [
        {
            "Numbers",
            [7,4,9,5,11,17,23,2,0,14,21,24,10,16,13,6,15,25,12,22,18,20,8,19,3,26,1]
        },
        {
            "Numbers Processed",
            []
        },
        {
            "Boards",
            #{
                0 => #{
                    total => 300,
                    rows =>     #{ 0 => {5, 63},     1 => {5, 61},    2 => {5, 67},    3 => {5, 42},    4 => {5, 67} },
                    columns =>  #{ 0 => {5, 58},     1 => {5, 46},    2 => {5, 77},    3 => {5, 64},    4 => {5, 55} }
                },
                1 => #{
                    total => 324,
                    rows =>     #{ 0 => {5, 42},     1 => {5, 62},    2 => {5, 82},    3 => {5, 69},    4 => {5, 69} },
                    columns =>  #{ 0 => {5, 65},     1 => {5, 73},    2 => {5, 46},    3 => {5, 80},    4 => {5, 60} }
                },
                2 => #{
                    total => 325,
                    rows =>     #{ 0 => {5, 80},     1 => {5, 69},    2 => {5, 95},    3 => {5, 57},    4 => {5, 24} },
                    columns =>  #{ 0 => {5, 66},     1 => {5, 56},    2 => {5, 80},    3 => {5, 68},    4 => {5, 55} }
                }
            }
        },
        {
            "Boards with Completes",
            []
        }],

    % ACT

    lists:foreach(fun (Input) -> ok = gen_server:cast(test_part1, Input) end, Data),
    ActualPart1 = gen_server:call(test_part1, result),

    % ASSERT

    ?assertEqual(ActualPart1, ExpectedPart1),

    % ARRANGE

    ExpectedPart2 = [
        {
            "Numbers",
            [10,16,13,6,15,25,12,22,18,20,8,19,3,26,1]
        },
        {
            "Numbers Processed",
            [24,21,14,0,2,23,17,11,5,9,4,7]
        },
        {
            "Boards",
            #{
                0 => #{
                    total => 163,
                    rows =>     #{ 0 => {2, 35},     1 => {1, 8},     2 => {1, 16},    3 => {4, 37},    4 => {5, 67} },
                    columns =>  #{ 0 => {4, 37},     1 => {3, 35},    2 => {2, 23},    3 => {3, 49},    4 => {1, 19} }
                },
                1 => #{
                    total => 187,
                    rows =>     #{ 0 => {3, 40},     1 => {2, 31},    2 => {3, 52},    3 => {2, 30},    4 => {3, 34} },
                    columns =>  #{ 0 => {3, 42},     1 => {3, 41},    2 => {3, 39},    3 => {2, 37},    4 => {2, 28} }
                },
                2 => #{
                    total => 188,
                    rows =>     #{ 0 => {0, 0},      1 => {4, 60},    2 => {4, 72},    3 => {3, 41},    4 => {2, 15} },
                    columns =>  #{ 0 => {3, 50},     1 => {2, 24},    2 => {3, 40},    3 => {3, 35},    4 => {2, 39} }
                }
            }
        },
        {
            "Boards with Completes",
            [2]
        }],

    % ACT

    lists:foreach(fun(_) -> gen_server:cast(test_part1, process) end,        
        lists:seq(1, 12)),
    ActualPart2 = gen_server:call(test_part1, result),

    % ASSERT

    ?assertEqual(ActualPart2, ExpectedPart2),

    % TEARDOWN

    gen_server:stop(test_part1).