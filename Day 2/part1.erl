-module(part1).
-behaviour(gen_server).
-export([
    init/1,
    handle_call/3,
    handle_cast/2
]).



init(_) ->
    {ok, {0, 0}}.



handle_call({_, Value}, _, State) when Value < 0 ->
    {reply, error, State};

handle_call({forward, Value}, _, State) ->
    {Distance, Depth} = State,
    {reply, ok, {Distance + Value, Depth}};

handle_call({up, Value}, _, {Distance, Depth}) when Value > Depth ->
    {reply, error, {Distance, Depth}};

handle_call({up, Value}, _, State) ->
    {Distance, Depth} = State,
    {reply, ok, {Distance, Depth - Value}};

handle_call({down, Value}, _, State) ->
    {Distance, Depth} = State,
    {reply, ok, {Distance, Depth + Value}};

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



handle_cast(_, State) ->
    {noreply, State}.
