-module(part1).
-behaviour(gen_server).

-include_lib("eunit/include/eunit.hrl").

-export([
    init/1,
    handle_call/3,
    handle_cast/2
]).

init(_) ->
    {ok, {#{}, {0, 0}}}.

handle_cast(reset, _) ->
    {noreply, {#{}, {0, 0}}};

handle_cast(process, State) ->
    {noreply, State};

handle_cast(Value, State) ->
    {Values, {Minimum, Volume}} = State,
    NewState =
    case Values of
        #{Value := ValueVolume} ->
            { Values#{Value := ValueVolume + 1}, {Minimum, Volume + 1} };
        _ ->
            { Values#{Value => 1},
            {
                if Value < Minimum -> Value; true -> Minimum end,
                Volume + 1
            } }
    end,
    {noreply, NewState}.

find_median(Values, Minimum, TargetVolume) ->
    find_median(Values, Minimum, 0, TargetVolume).

find_median(_, CurrentValue, CurrentVolume, TargetVolume) when CurrentVolume >= TargetVolume ->
    CurrentValue;

find_median(Values, CurrentValue, CurrentVolume, TargetVolume) ->
    NextValue = CurrentValue + 1,
    Increment = 
    case Values of
        #{NextValue := NextValueVolume} ->
            NextValueVolume;
        _ ->
            0
    end,
    find_median(Values, NextValue, CurrentVolume + Increment, TargetVolume).

find_movement(Values, Median) ->
    lists:sum(
        maps:values(
            maps:map(fun (Key, Value) -> abs(Key - Median) * Value end, Values)
        )
    ).

handle_call(result, _, State) ->
    {Values, {Minimum, Volume}} = State,
    TargetVolume = (Volume + 1) div 2,
    Median = find_median(Values, Minimum, TargetVolume),
    {
        reply,
        [
            { "Median horizontal value", Median },
            { "Movement", find_movement(Values, Median) },
            
            { "Movement", find_movement(Values, Median - 1) },
            
            { "Movement", find_movement(Values, Median - 2) },
            
            { "Movement", find_movement(Values, Median - 3) },
            
            { "Movement", find_movement(Values, Median - 4) },

            
            { "Movement", find_movement(Values, Median - 5) },

            
            { "Movement", find_movement(Values, Median - 6) },
            { "Aggregates", {Minimum, Volume} }
        ],
        State
    }.

% Main test function for part1.

part1_test() ->

    % SETUP

    TryResult = gen_server:start_link({local, test_part1}, part1, [], []),
    Result = case TryResult of
        {error, {already_started, _}} ->
            gen_server:stop(test_part1),
            gen_server:start_link({local, test_part1}, part1, [], []);
        _ -> TryResult
    end,
    {ok, _} = Result,

    % ARRANGE

    Data = [16,1,2,0,4,2,7,1,2,14],
    ExpectedPart1 = [ { "Median horizontal value", 2 }, { "Movement", 37 } ],

    % ACT

    lists:foreach(fun (Input) -> ok = gen_server:cast(test_part1, Input) end, Data),
    ActualPart1 = gen_server:call(test_part1, result),

    % ASSERT

    ?assertEqual(ActualPart1, ExpectedPart1).
