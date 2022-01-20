-module(part1).
-behaviour(gen_server).

-export([
    init/1,
    handle_cast/2,
    handle_call/3
]).

%
% Server state is a map containing -
% numbers (list of numbers to process)
% numbers_processed (list of numbers already processed)
% boards (all the totals for each board).

init(_) ->
    {
        ok,
        #{
            numbers => [],
            numbers_processed => [],
            boards => #{}
        }
    }.

handle_cast({numbers, NumbersToAdd}, State) ->
    #{numbers := Numbers} = State,
    {
        noreply,
        State#{
            numbers := Numbers ++ NumbersToAdd
        }
    };

handle_cast({board, BoardIndex, RowIndex, ColumnValues}, State) when length(ColumnValues) == 5 ->
    #{boards := Boards} = State,
    Board = case Boards of
        #{BoardIndex := BoardCurrent} -> BoardCurrent;
        _ -> #{
            total => 0,
            rows => #{0 => 0, 1 => 0, 2 => 0, 3 => 0, 4 => 0},
            columns => #{0 => 0, 1 => 0, 2 => 0, 3 => 0, 4 => 0}
        }
    end,
    % New total
    #{total := Total, rows := Rows, columns: Columns} = Board,
    % Next part is to unwrap more and adjust correct values, I think it is just a case of you
    % getting your head round the map syntax.

handle_cast(process, State) ->
    #{
        numbers := [NumberNext | NumbersRest],
        numbers_processed := NumbersProcessed
    } = State,
    % TODO - do something with the number here
    {
        noreply,
        State#{
            numbers := NumbersRest,
            numbers_processed := [NumberNext | NumbersProcessed]
        }
    }.





