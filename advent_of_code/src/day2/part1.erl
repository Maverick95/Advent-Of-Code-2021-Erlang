-module(part1).
-behaviour(gen_server).
-export([
    init/1,
    handle_call/3,
    handle_cast/2
]).

init(_) ->
    {ok, {0, 0}}.

handle_cast({forward, Value}, {Distance, Depth}) when Value > 0 ->
    {noreply, {Distance + Value, Depth}};

handle_cast({up, Value}, {Distance, Depth}) when Value > 0, Value =< Depth ->
    {noreply, {Distance, Depth - Value}};

handle_cast({down, Value}, {Distance, Depth}) when Value > 0 ->
    {noreply, {Distance, Depth + Value}};

handle_cast(reset, _) ->
    {noreply, {0, 0}}.

handle_call(result, _, State) ->
    {Distance, Depth} = State,
    {
        reply,
        [
            {"Distance", Distance},
            {"Depth", Depth},
            {"Distance x Depth", Distance * Depth}
        ],
        State
    }.
