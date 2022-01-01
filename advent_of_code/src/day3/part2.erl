-module(part2).
-behaviour(gen_server).
-export([
    init/1,
    handle_call/3,
    handle_cast/2
]).



init(_) ->
    {ok, {unset, []}}.



handle_cast({L, V}, {unset, []}) ->
    {noreply, {L, [V]}};

handle_cast({L, _}, {Length, Values}) when L /= Length ->
    {noreply, {Length, Values}};

handle_cast({_, V}, {Length, Values}) ->
    {noreply, {Length, [V | Values]}};



handle_cast(reset, _) ->
    {noreply, {unset, []}}.



handle_call(result, _, {Length, Values}) ->
    Oxygen = get_aggr_bit_sequence(Values, most, Length - 1),
    Co2 = get_aggr_bit_sequence(Values, least, Length - 1),
    {
        reply,
        [
            {"Oxygen", Oxygen},
            {"CO2", Co2}
        ],
        {Length, Values}
    }.



get_aggr_bit_sequence([], _, _) ->
    error;

get_aggr_bit_sequence(Values, _, -1) when length(Values) > 1 ->
    error;

get_aggr_bit_sequence(Values, _, _) when length(Values) == 1 ->
    [Value] = Values,
    Value;

get_aggr_bit_sequence(Values, Type, N) ->
    Count = length(Values),
    Bitvalues = lists:map(fun(Value) -> get_bitvalue(Value, N) end, Values),
    Sum = lists:sum(Bitvalues),
    Status = {Sum, Count - Sum},
    Filtered = lists:filter(filter_value(Status, N, Type), Values),
    get_aggr_bit_sequence(Filtered, Type, N - 1).



get_bitvalue(Value, N) ->
    Bitvalue = Value band (1 bsl N),
    if
        Bitvalue > 0 -> 1;
        true -> 0
    end.



filter_value({Ones, Zeroes}, N, Type) ->
    Filter =
        case Type of
            most ->
                if
                    Ones >= Zeroes -> 1;
                    true -> 0
                end;
            least ->
                if
                    Ones >= Zeroes -> 0;
                    true -> 1
                end
        end,

    fun(Value) ->
        case get_bitvalue(Value, N) of
            Filter -> true;
            _ -> false
        end
    end.
