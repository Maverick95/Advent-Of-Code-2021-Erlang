-module(input_handler).
-behaviour(gen_event).

-export([
    init/1,
    handle_event/2,
    handle_call/2
]).



init(State) ->
    {ok, State}.



handle_event(Data, State) ->
    {Server, Manager} = State,
    Transform = gen_server:call(Server, Data),
    case Transform of
        error ->
            {ok, State};
        _ ->
            gen_event:notify(Manager, Transform),
            {ok, State}
    end.



handle_call(_, State) ->
    {ok, error, State}.
