-module(prime_sup).
-behaviour(supervisor).
-author("Ivan Blinkov <ivan@blinkov.ru>").
-compile([{parse_transform, lager_transform}]).

-export([start_link/0]).
-export([init/1]).

start_link() ->
	supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->
	lager:info("Starting prime supervisor"),
	{ok, {{one_for_one, 10, 10}, []}}.
