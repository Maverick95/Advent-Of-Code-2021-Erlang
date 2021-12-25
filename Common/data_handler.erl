-module(data_handler).
-behaviour(gen_event).

-export([
    init/1,
    handle_call/2,
    handle_event/2,
    terminate/2
]).

init({Server, Logger}) ->
    {_, PidServer} = gen_server:start_link({local, Server}, Server, [], []),
    {ok, {PidServer, Logger}}.

handle_call(_, State) ->
    {ok, error, State}.

handle_event(result, State) ->
    {PidServer, Logger} = State,
    Reply = gen_server:call(PidServer, result),
    gen_event:notify(Logger, Reply),
    {ok, State};

handle_event(Event, State) ->
    {PidServer, _} = State,
    gen_server:call(PidServer, Event),
    {ok, State}.

terminate(_, {PidServer, _}) ->
    gen_server:stop(PidServer).
