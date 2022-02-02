-module(part1).
-behaviour(gen_server).
-export([
    init/1,
    handle_call/3,
    handle_cast/2
]).

init(_) ->
    {ok, {#{}, 0}}.

handle_cast({C1, C2}, State) when C1 == C2 ->
    NewState = handle_update(C1, {0, 0}, 0, State),
    {noreply, NewState};

handle_cast({{X1, Y1}, {X2, Y2}}, State) when X1 == X2, Y1 /= Y2 ->
    Diff = if Y1 < Y2 -> 1; true -> -1 end,
    NewState = handle_update({X1, Y1}, {0, Diff}, abs(Y2 - Y1), State),
    {noreply, NewState};

handle_cast({{X1, Y1}, {X2, Y2}}, State) when Y1 == Y2, X1 /= X2 ->
    Diff = if X1 < X2 -> 1; true -> -1 end,
    NewState = handle_update({X1, Y1}, {Diff, 0}, abs(X2 - X1), State),
    {noreply, NewState};

handle_cast(reset, _) ->
    {noreply, {#{}, 0}}.

handle_call(result, _, State) ->
    {Map, Volume} = State,
    {
        reply,
        [
            {"Map", Map},
            {"Coordinates appearing at least twice", Volume}
        ],
        State
    }.

handle_update(C, D, DiffCountMax, State) ->
    handle_update(C, D, 0, DiffCountMax, State).

handle_update(_, _, DiffCount, DiffCountMax, State) when DiffCount > DiffCountMax ->
    State;

handle_update({X, Y}, Diff, DiffCount, DiffCountMax, State) ->
    {DiffX, DiffY} = Diff,
    {Map, Volume} = State,
    Count =
        case maps:find({X, Y}, Map) of
            {ok, Value} ->
                Value;
            error ->
                0
        end,
    NewCount = Count + 1,
    NewMap = Map#{{X, Y} => NewCount},
    NewVolume =
        case NewCount of
            2 ->
                Volume + 1;
            _ ->
                Volume
        end,
    handle_update({X + DiffX, Y + DiffY}, Diff, DiffCount + 1, DiffCountMax, {NewMap, NewVolume}).
