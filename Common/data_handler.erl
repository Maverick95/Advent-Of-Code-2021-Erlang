-module(data_handler).
-behaviour(gen_event).

-export([
    init/1,
    handle_call/2,
    handle_event/2
]).

init(State) ->
    {ok, State}.

handle_call(_, State) ->
    {ok, error, State}.

handle_event(result, State) ->
    {Server, Logger} = State,
    Reply = gen_server:call(Server, result),
    gen_event:notify(Logger, Reply),
    {ok, State};

handle_event(Event, State) ->
    {Server, _} = State,
    gen_server:call(Server, Event),
    {ok, State}.

