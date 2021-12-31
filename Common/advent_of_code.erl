-module(advent_of_code).
-behaviour(application).
-export([
    start/2,
    stop/1
]).

start(_, Args) ->
    supervisor:start_link(main_supervisor, Args).

stop(_) ->
    ok.