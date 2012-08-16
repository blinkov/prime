-module(prime).
-author("Ivan Blinkov <ivan@blinkov.ru>").
-compile([{parse_transform, lager_transform}]).

-behaviour(application).
-export([start/0, start/2, stop/1]).
-export([get_progress/0, save/0]).
-export([config/1, reload/0, reload_config/0]).

-include("include/constants.hrl").

start() ->
	application:start(sync),
	application:start(?MODULE).

start(_Type, _Args) ->
    Supervisor = prime_sup:start_link(),
	MaxN = ?MODULE:config(n),	
	Seq = lists:seq(1, MaxN div ?BATCH_SIZE + 1),
	
	ets:new(?PRIMES, ?ETS_OPTIONS),
	ets:new(?WORKERS, ?ETS_OPTIONS),
	
	lists:map(fun(WorkerNumber) ->
		WorkerRef = generate_worker_ref(WorkerNumber),
		UpperBoundaryCandidate = (WorkerNumber * ?BATCH_SIZE + 1),
		UpperBoundary = case UpperBoundaryCandidate > MaxN of
			true -> MaxN;
			false -> UpperBoundaryCandidate
		end,
		Boundaries = {(WorkerNumber - 1) * ?BATCH_SIZE + 2, UpperBoundary},
		supervisor:start_child(prime_sup,
			{WorkerRef, {worker_server, start_link, 
			[WorkerRef, Boundaries]},
     		permanent, 5000, worker, [worker_server]}),
		worker_server:start_work(WorkerRef),
		ets:insert(?WORKERS, {WorkerRef, Boundaries})
	end, Seq),
	Supervisor.

stop(_State) ->
	lager:critical("Application shutting down"),
    ok.


get_progress() ->
	Workers = ets:match_object(?WORKERS, '_'),
	FloatProgress = lists:sum(lists:map(fun({WorkerRef, _}) ->
		worker_server:get_progress(WorkerRef)
	end, Workers)) / length(Workers),
	io:format("~p\%~n", [trunc(FloatProgress * 100)]).

save() ->
	FileName = prime:config(output),
	{ok, File} = file:open(FileName, [raw, write]),
	lists:map(fun({N}) ->
		file:write(File, integer_to_list(N)),
		file:write(File, <<", ">>)
	end, ets:match_object(?PRIMES, '_')),
	file:close(File),
	lager:info("Saved results to ~p", [FileName]).	

config(Key) ->
	case application:get_env(prime, Key) of
		{ok, Value} ->
			Value;
		undefined ->
			undefined
	end.

reload() ->
	sync_scanner:rescan(),
	?MODULE:reload_config(),
	ok.

reload_config() ->
	case file:consult("prime.config") of
		{ok, [Terms]} ->
			Terms;
		{error, enoent} ->
			{ok, [Terms]} = file:consult("../prime.config")
	end,
	lists:map(fun({App, Config}) ->
		lists:map(fun({Key, Value}) ->
			application:set_env(App, Key, Value)
		end, Config)
	end, Terms).

generate_worker_ref(WorkerNumber) ->
	% I know line below is unsafe, just don't want to make it more complicated
	list_to_atom("worker_" ++ integer_to_list(WorkerNumber)).