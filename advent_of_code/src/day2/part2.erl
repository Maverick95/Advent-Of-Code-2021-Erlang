-module(part2).
-behaviour(gen_server).
-export([
    init/1,
    handle_call/3,
    handle_cast/2
]).

init(_) ->
    {ok, {0, 0, 0}}.

handle_cast({forward, Value}, {Distance, Depth, Aim}) when Value > 0 ->
    {noreply, {Distance + Value, Depth + Aim * Value, Aim}};

handle_cast({up, Value}, {Distance, Depth, Aim}) when Value > 0, Value =< Aim ->
    {noreply, {Distance, Depth, Aim - Value}};

handle_cast({down, Value}, {Distance, Depth, Aim}) when Value > 0 ->
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
            {"Aim", Aim},
            {"Distance x Depth", Distance * Depth}
        ],
        State
    }.
