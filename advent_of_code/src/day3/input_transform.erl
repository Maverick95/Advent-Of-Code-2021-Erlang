-module(input_transform).
-behaviour(gen_server).

-export([
    init/1,
    handle_call/3,
    handle_cast/2
    ]).



init(_) ->
    {ok, none}.



handle_call(Input, _, State) ->
    {reply, get_integer(lists:reverse(Input)), State}.



get_integer(Bitstring) ->
    get_integer(Bitstring, 1, 0, 0).

get_integer([], _, Length, Total) ->
    {Length, Total};

get_integer([Head | Rest], Value, Length, Total) ->
    case Head of
        48 -> % 0
            get_integer(Rest, Value * 2, Length + 1, Total);
        49 -> % 1
            get_integer(Rest, Value * 2, Length + 1, Total + Value);
        _ ->
            error
    end.



handle_cast(_, State) ->
    {noreply, State}.