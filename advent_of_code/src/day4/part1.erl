-module(part1).
%-behaviour(gen_server).

-include_lib("eunit/include/eunit.hrl").

-export([
    init/1,
    handle_cast/2
    %,
    %handle_call/3
]).

% Initialize

init(_) ->
    {
        ok,
        #{
            numbers => [],
            numbers_processed => [],
            boards => #{},
            lookups => #{}
        }
    }.

% Handle numbers events

handle_cast({numbers, NumbersToAdd}, State) ->
    #{numbers := Numbers} = State,
    {
        noreply,
        State#{
            numbers := Numbers ++ NumbersToAdd
        }
    };

% Handle board events

handle_cast({board, BoardIndex, RowIndex, ColumnValues}, State) ->
    #{boards := Boards, lookups := Lookups} = State,
    Board = case Boards of
        #{BoardIndex := BoardCurrent} -> BoardCurrent;
        _ -> #{
            total => 0,
            rows => #{0 => 0, 1 => 0, 2 => 0, 3 => 0, 4 => 0},
            columns => #{0 => 0, 1 => 0, 2 => 0, 3 => 0, 4 => 0}
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
        lookups := Lookups
    } = State,

    StateNew = case Numbers of
        [NumbersNext | NumbersRest] ->
            NotProcessed = lists:all(fun(X) -> X /= NumbersNext end, NumbersProcessed),
            if
                NotProcessed ->
                    BoardsNew = lookup_and_subtract(NumbersNext, Lookups, Boards),
                    State#{
                        boards := BoardsNew,
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
    RowTotal = 0,
    Index = 0,
    update_board_totals(Total, RowTotal, Columns, Index, ColumnValues).

update_board_totals(Total, RowTotal, Columns, Index, [ColumnValuesNext | ColumnValuesRest]) ->
    #{Index := ColumnTotal} = Columns,
    update_board_totals(
        Total + ColumnValuesNext,
        RowTotal + ColumnValuesNext,
        Columns#{Index := ColumnTotal + ColumnValuesNext},
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
            100,    #{0 => 0, 1 => 1, 2 => 2, 3 => 3, 4 => 4},      [10,15,20,25,30],
            % Outputs
            {
                200,
                100,
                #{0 => 10, 1 => 16, 2 => 22, 3 => 28, 4 => 34}
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

lookup_and_subtract(Number, Lookups, Boards) ->
    case Lookups of
        #{Number := Squares} ->
            subtract(Number, Squares, Boards);
        _ ->
            Boards
    end.

subtract(_, [], Boards) ->
    Boards;

subtract(Number, [{BoardIndex, RowIndex, ColumnIndex} | SquaresRest], Boards) ->
    
    #{BoardIndex := Board} = Boards,
    #{total := Total, rows := Rows, columns:= Columns} = Board,
    #{RowIndex := RowTotal} = Rows,
    #{ColumnIndex := ColumnTotal} = Columns,
    
    BoardNew = Board#{
        total := Total - Number,
        rows := Rows#{RowIndex := RowTotal - Number},
        columns := Columns#{ColumnIndex := ColumnTotal - Number}
        },

    BoardsNew = Boards#{
        BoardIndex := BoardNew
        },

    subtract(Number, SquaresRest, BoardsNew).
