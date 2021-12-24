-module(part1).
-behaviour(gen_server).
-export([
    init/1,
    handle_call/3,
    handle_cast/2
]).



init(_) ->
    {ok, {#{}, 0}}.



handle_call({C1, C2}, _, State) when C1 == C2 ->
    NewState = handle_update(C1, {0, 0}, 0, State),
    {reply, ok, NewState};

handle_call({{X1, Y1}, {X2, Y2}}, _, State) when X1 == X2 ->
    Diff = if Y1 < Y2 -> 1; true -> -1 end,
    NewState = handle_update({X1, Y1}, {0, Diff}, abs(Y2 - Y1), State),
    {reply, ok, NewState};

handle_call({{X1, Y1}, {X2, Y2}}, _, State) when Y1 == Y2 ->
    Diff = if X1 < X2 -> 1; true -> -1 end,
    NewState = handle_update({X1, Y1}, {Diff, 0}, abs(X2 - X1), State),
    {reply, ok, NewState};

handle_call(result, _, State) ->
    {Map, Volume} = State,
    {
        reply,
        [
            {"Map", Map},
            {"Volume", Volume}
        ],
        State
    };

handle_call(_, _, State) ->
    {reply, ok, State}.



handle_update(C, D, Increases, State) ->
    handle_update(C, D, 0, Increases, State).

handle_update(_, _, Index, Increases, State) when Index > Increases ->
    State;

handle_update({X, Y}, {DiffX, DiffY}, Index, Increases, State) ->
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
    handle_update({X + DiffX, Y + DiffY}, {DiffX, DiffY}, Index + 1, Increases, {NewMap, NewVolume}).



handle_cast(quit, State) ->
    {stop, normal, State}.