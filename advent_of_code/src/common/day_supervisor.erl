-module(day_supervisor).
-behaviour(supervisor).

-export([
    init/1
]).

init({ [Transform | Parts], TransformId, PartsPrefix }) ->
    TransformConfigs =
        #{
            id => sup_input_server,
            start => {gen_server, start_link, [{local, TransformId}, Transform, [], []]}
        },
    PartsConfigs = parts_configs(Parts, PartsPrefix),
    {
        ok,
        {
            #{ strategy => one_for_all },
            [TransformConfigs | PartsConfigs]
        }
    }.

parts_configs(Parts, PartsPrefix) ->
    Details = parts_details(Parts, PartsPrefix),
    lists:map(
        fun({SupId, Id, Module}) ->
        #{
            id => SupId,
            start => {gen_server, start_link, [{local, Id}, Module, [], []]}
        } end, Details).

parts_details(Parts, PartsPrefix) ->
    parts_details(Parts, PartsPrefix, [], 0).

parts_details([], _, Current, _) -> Current;

parts_details([Module | Rest], PartsPrefix, Current, Index) ->
    Suffix = integer_to_list(Index),
    SupId = list_to_atom("sup_data_server_" ++ Suffix),
    Id = list_to_atom(PartsPrefix ++ "_" ++ Suffix),
    Next = {SupId, Id, Module},
    parts_details(Rest, PartsPrefix, [Next | Current], Index + 1).