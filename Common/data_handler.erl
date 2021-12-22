-module(data_handler).
-behaviour(gen_event).

-export([
    init/1,
    handle_call/2,
    handle_event/2,
    terminate/2
]).

init({Mod, Server, Logger}) ->
    {_, PidServer} = gen_server:start_link({local, Server}, Mod, [], []),
    {_, PidLogger} = gen_event:start_link({local, Logger}),
    gen_event:add_handler(PidLogger, data_logger, Logger),
    {ok, [PidServer, PidLogger]}.

handle_call(_, State) ->
    {ok, error, State}.

handle_event(result, State) ->
    [PidServer, PidLogger] = State,
    Reply = gen_server:call(PidServer, result),
    gen_event:notify(PidLogger, Reply),
    {ok, State};

handle_event(Event, State) ->
    [PidServer, _] = State,
    % Note - exceptions not handled here.
    gen_server:call(PidServer, Event),
    {ok, State}.

terminate(_, [PidServer, PidLogger]) ->
    gen_server:cast(PidServer, quit),
    gen_event:stop(PidLogger).