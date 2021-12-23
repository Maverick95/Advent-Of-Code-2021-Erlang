-module(part1).
-behaviour(gen_server).
-export([
    init/1,
    handle_call/3,
    handle_cast/2
]).



init(_) ->
    {ok, {#{}, 0}}.



handle_call({{X1, Y1}, {X2, Y2}}, _, State) when X1 =< X2, Y1 == Y2 ->
    NewState = handle_update_x(Y1, X1, X2, State),
    {reply, ok, NewState};

handle_call({{X1, Y1}, {X2, Y2}}, _, State) when X1 > X2, Y1 == Y2 ->
    NewState = handle_update_x(Y1, X2, X1, State),
    {reply, ok, NewState};

handle_call({{X1, Y1}, {X2, Y2}}, _, State) when X1 == X2, Y1 =< Y2 ->
    NewState = handle_update_y(X1, Y1, Y2, State),
    {reply, ok, NewState};

handle_call({{X1, Y1}, {X2, Y2}}, _, State) when X1 == X2, Y1 > Y2 ->
    NewState = handle_update_y(X1, Y2, Y1, State),
    {reply, ok, NewState};

handle_call(_, _, State) ->
    {reply, ok, State}.



handle_update_x(Y, X1, X2, State) ->
    State.

handle_update_y(X, Y1, Y2, State) ->
    State.



handle_cast(quit, State) ->
    {stop, normal, State}.