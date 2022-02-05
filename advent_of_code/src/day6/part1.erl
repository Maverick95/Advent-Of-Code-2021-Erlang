-module(part1).
-behaviour(gen_server).

-include_lib("eunit/include/eunit.hrl").

-export([
    init/1,
    handle_cast/2,
    handle_call/3
]).

init(_) ->
    {ok, { #{}, 0 }}.


handle_cast(reset, _) ->
    {noreply, { #{}, 0 }};

handle_cast(process, State) ->
    {noreply, move_forward(State)};

handle_cast(Input, State) ->
    {Volumes, Total} = State,
    NewVolumes = case Volumes of
        #{Input := Count} ->
            Volumes#{Input := Count + 1};
        _ ->
            Volumes#{Input => 1}
    end,
    {noreply, {NewVolumes, Total + 1}}.

move_forward(State) ->
    {Volumes, Total} = State,
    {NextVolumes, TotalIncrement} =
        case Volumes of
            #{0 := Count} ->
                { #{6 => Count, 8 => Count}, Count };
            _ ->
                { #{6 => 0, 8 => 0}, 0 }
        end,
    move_forward(1, State, {NextVolumes, Total + TotalIncrement}).

move_forward(Timer, State, NextState) when Timer >= 1, Timer =< 8 ->
    {Volumes, _} = State,
    {NextVolumes, NextTotal} = NextState,
    CurrentCount = case Volumes of
        #{Timer := CurrentLookup} ->
            CurrentLookup;
        _ ->
            0
    end,
    NextCount = case NextVolumes of
        #{Timer - 1 := NextLookup} ->
            NextLookup;
        _ ->
            0
    end,
    NewNextCount = CurrentCount + NextCount,
    NewNextVolumes = NextVolumes#{Timer - 1 => NewNextCount},
    move_forward(Timer + 1, State, {NewNextVolumes, NextTotal});

move_forward(9, _, NextState) ->
    NextState.

move_forward_test_() ->
    lists:map(
        fun({Current, Expected}) -> ?_assertEqual(Expected, move_forward(Current)) end,
        [
            {
                { #{1 => 1, 2 => 1, 3 => 2, 4 => 1}, 5 },
                { #{0 => 1, 1 => 1, 2 => 2, 3 => 1, 4 => 0, 5 => 0, 6 => 0, 7 => 0, 8 => 0}, 5 }
            },
            {
                { #{0 => 1, 1 => 1, 2 => 2, 3 => 1, 4 => 0, 5 => 0, 6 => 0, 7 => 0, 8 => 0}, 5 },
                { #{0 => 1, 1 => 2, 2 => 1, 3 => 0, 4 => 0, 5 => 0, 6 => 1, 7 => 0, 8 => 1}, 6 }
            }
        ]).

handle_call(result, _, State) ->
    {_, Total} = State,
    {
        reply,
        [
            { "Number of fish", Total }
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

    Data = [3,4,3,1,2],
    ExpectedPart1 = [ { "Number of fish", 5 } ],

    % ACT

    lists:foreach(fun (Input) -> ok = gen_server:cast(test_part1, Input) end, Data),
    ActualPart1 = gen_server:call(test_part1, result),

    % ASSERT

    ?assertEqual(ActualPart1, ExpectedPart1),

    % ARRANGE

    ExpectedPart2 = [ { "Number of fish", 26 } ],

    % ACT

    lists:foreach(fun(_) -> gen_server:cast(test_part1, process) end,        
        lists:seq(1, 18)),
    ActualPart2 = gen_server:call(test_part1, result),

    % ASSERT

    ?assertEqual(ActualPart2, ExpectedPart2),

     % ARRANGE

    ExpectedPart3 = [ { "Number of fish", 5934 } ],

    % ACT

    lists:foreach(fun(_) -> gen_server:cast(test_part1, process) end,        
        lists:seq(1, 62)),
    ActualPart3 = gen_server:call(test_part1, result),

    % ASSERT

    ?assertEqual(ActualPart3, ExpectedPart3),

    % TEARDOWN

    gen_server:stop(test_part1).
