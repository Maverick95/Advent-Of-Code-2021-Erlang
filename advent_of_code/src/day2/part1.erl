-module(part1).
-behaviour(gen_server).
-export([
    init/1,
    handle_call/3,
    handle_cast/2
]).



init(_) ->
    {ok, {0, 0}}.



handle_cast({_, Value}, State) when Value < 0 ->
    {noreply, State};

handle_cast({forward, Value}, State) ->
    {Distance, Depth} = State,
    {noreply, {Distance + Value, Depth}};

handle_cast({up, Value}, {Distance, Depth}) when Value > Depth ->
    {noreply, {Distance, Depth}};

handle_cast({up, Value}, State) ->
    {Distance, Depth} = State,
    {noreply, {Distance, Depth - Value}};

handle_cast({down, Value}, State) ->
    {Distance, Depth} = State,
    {noreply, {Distance, Depth + Value}}.



handle_call(result, _, State) ->
    {Distance, Depth} = State,
    {
        reply,
        [
            {"Distance", Distance},
            {"Depth", Depth}
        ],
        State
    }.