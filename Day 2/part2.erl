-module(part2).
-behaviour(gen_server).
-export([
    init/1,
    handle_call/3,
    handle_cast/2
]).

init(_) ->
    {ok, {0, 0, 0}}.

handle_call({_, Value}, _, State) when Value < 0 ->
    {reply, error, State};

handle_call({forward, Value}, _, {Distance, Depth, Aim}) when Depth + Aim * Value < 0 ->
    {reply, error, {Distance, Depth, Aim}};

handle_call({forward, Value}, _, State) ->
    {Distance, Depth, Aim} = State,
    {reply, ok, {Distance + Value, Depth + Aim * Value, Aim}};

handle_call({up, Value}, _, State) ->
    {Distance, Depth, Aim} = State,
    {reply, ok, {Distance, Depth, Aim - Value}};

handle_call({down, Value}, _, State) ->
    {Distance, Depth, Aim} = State,
    {reply, ok, {Distance, Depth, Aim + Value}};

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

handle_cast(quit, State) ->
    {stop, normal, State}.


