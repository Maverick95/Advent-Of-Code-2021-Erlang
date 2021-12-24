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
    {X1, Y1} = C1,
    NewState = handle_update_x(Y1, X1, X1, State),
    {reply, ok, NewState};

handle_call({{X1, Y1}, {X2, Y2}}, _, State) when X1 == X2 ->
    {O1, O2} = order(Y1, Y2),
    NewState = handle_update_y(X1, O1, O2, State),
    {reply, ok, NewState};

handle_call({{X1, Y1}, {X2, Y2}}, _, State) when Y1 == Y2 ->
    {O1, O2} = order(X1, X2),
    NewState = handle_update_x(Y1, O1, O2, State),
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



order(V1, V2) ->
    if
        V1 =< V2 ->
            {V1, V2};
        true ->
            {V2, V1}
    end.



handle_update_x(_, X1, X2, State) when X1 > X2 ->
    State;

handle_update_x(Y, X1, X2, State) ->
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
    handle_update_x(Y, X1 + 1, X2, {NewMap, NewVolume}).



handle_update_y(_, Y1, Y2, State) when Y1 > Y2 ->
    State;

handle_update_y(X, Y1, Y2, State) ->
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
    handle_update_y(X, Y1 + 1, Y2, {NewMap, NewVolume}).



handle_cast(quit, State) ->
    {stop, normal, State}.