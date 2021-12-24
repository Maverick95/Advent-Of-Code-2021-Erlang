-module(part2).
-behaviour(gen_server).
-export([
    init/1,
    handle_call/3,
    handle_cast/2
]).



init(_) ->
    {ok, {#{}, 0}}.



handle_call({C1, C2}, _, State) when C1 == C2 ->
    {X1, Y1} = C1,
    NewState = handle_update_x(Y1, X1, X1, 1, State),
    {reply, ok, NewState};

handle_call({{X1, Y1}, {X2, Y2}}, _, State) when X1 == X2 ->
    Diff = if Y1 =< Y2 -> 1; true -> -1 end,
    NewState = handle_update_y(X1, Y1, Y2, Diff, State),
    {reply, ok, NewState};

handle_call({{X1, Y1}, {X2, Y2}}, _, State) when Y1 == Y2 ->
    Diff = if X1 =< X2 -> 1; true -> -1 end,
    NewState = handle_update_x(Y1, X1, X2, Diff, State),
    {reply, ok, NewState};

handle_call({{X1, Y1}, {X2, Y2}}, _, State) when abs(X2 - X1) == abs(Y2 - Y1) ->
    DiffX = if X1 =< X2 -> 1; true -> -1 end,
    DiffY = if Y1 =< Y2 -> 1; true -> -1 end,
    NewState = handle_update_xy(X1, X2, Y1, DiffX, DiffY, State),
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



handle_update_x(Y, X1, X2, Diff, State) ->
    {Map, Volume} = State,
    Count =
        case maps:find({X1, Y}, Map) of
            {ok, Value} ->
                Value;
            error ->
                0
        end,
    NewCount = Count + 1,
    NewMap = Map#{{X1, Y} => NewCount},
    NewVolume =
        case NewCount of
            2 ->
                Volume + 1;
            _ ->
                Volume
        end,
    if
        X1 == X2 ->
            {NewMap, NewVolume};
        true ->
            handle_update_x(Y, X1 + Diff, X2, Diff, {NewMap, NewVolume})
    end.



handle_update_y(X, Y1, Y2, Diff, State) ->
    {Map, Volume} = State,
    Count =
        case maps:find({X, Y1}, Map) of
            {ok, Value} ->
                Value;
            error ->
                0
        end,
    NewCount = Count + 1,
    NewMap = Map#{{X, Y1} => NewCount},
    NewVolume =
        case NewCount of
            2 ->
                Volume + 1;
            _ ->
                Volume
        end,
    if
        Y1 == Y2 ->
            {NewMap, NewVolume};
        true ->
            handle_update_y(X, Y1 + Diff, Y2, Diff, {NewMap, NewVolume})
    end.



handle_update_xy(X1, X2, Y, DiffX, DiffY, State) ->
    {Map, Volume} = State,
    Count =
        case maps:find({X1, Y}, Map) of
            {ok, Value} ->
                Value;
            error ->
                0
        end,
    NewCount = Count + 1,
    NewMap = Map#{{X1, Y} => NewCount},
    NewVolume =
        case NewCount of
            2 ->
                Volume + 1;
            _ ->
                Volume
        end,
    if
        X1 == X2 ->
            {NewMap, NewVolume};
        true ->
            handle_update_xy(X1 + DiffX, X2, Y + DiffY, DiffX, DiffY, {NewMap, NewVolume})
    end.



handle_cast(quit, State) ->
    {stop, normal, State}.