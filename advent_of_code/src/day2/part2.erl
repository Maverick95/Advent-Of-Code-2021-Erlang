-module(part2).
-behaviour(gen_server).
-export([
    init/1,
    handle_call/3,
    handle_cast/2
]).



init(_) ->
    {ok, {0, 0, 0}}.



handle_cast({_, Value}, State) when Value < 0 ->
    {noreply, State};

handle_cast({forward, Value}, {Distance, Depth, Aim}) when Depth + Aim * Value < 0 ->
    {noreply, {Distance, Depth, Aim}};

handle_cast({forward, Value}, State) ->
    {Distance, Depth, Aim} = State,
    {noreply, {Distance + Value, Depth + Aim * Value, Aim}};

handle_cast({up, Value}, State) ->
    {Distance, Depth, Aim} = State,
    {noreply, {Distance, Depth, Aim - Value}};

handle_cast({down, Value}, State) ->
    {Distance, Depth, Aim} = State,
    {noreply, {Distance, Depth, Aim + Value}};



handle_cast(reset, _) ->
    {noreply, {0, 0, 0}}.



handle_call(result, _, State) ->
    {Distance, Depth, Aim} = State,
    {
        reply,
        [
            {"Distance", Distance},
            {"Depth", Depth},
            {"Aim", Aim}
        ],
        State
    }.
