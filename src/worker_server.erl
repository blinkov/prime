-module(worker_server).
-behaviour(gen_server).
-author("Ivan Blinkov <ivan@blinkov.ru>").

-compile([{parse_transform, lager_transform}]).

-export([start_work/1, get_progress/1]).
-export([start_link/2]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-include("include/constants.hrl").

-record(state, {first, last, current}).

start_work(ServerRef) ->
	gen_server:call(ServerRef, start_work).
	
get_progress(ServerRef) ->
	gen_server:call(ServerRef, get_progress).	

start_link(ServerRef, Args) ->
    gen_server:start_link({local, ServerRef}, ?MODULE, Args, []).

init({First, Last}) ->
	lager:info("Worker started"),
	{ok, #state{first = First, last = Last, current = Last}}.

handle_call(start_work, _From, State) ->
	gen_server:cast(self(), {is_prime, State#state.first}),
	{reply, ok, State};

handle_call(get_progress, _From, 
			State = #state{first = First, current = Current, last = Last}) ->
	{reply, (Current - First) / (Last - First), State};

handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.

handle_cast({is_prime, N}, State) ->
	if
		N =< State#state.last ->
			case miller_rabin:is_prime(N) of
				true ->
					lager:debug("~p is prime", [N]),
					ets:insert(?PRIMES, {N});
				false ->
					lager:debug("~p is composite", [N])
			end,
			gen_server:cast(self(), {is_prime, N + 1});
		true ->
			lager:info("Worker completed it's job!")
	end,
	{noreply, State#state{current = N + 1}};

handle_cast(stop, State) ->
	{stop, normal, State};

handle_cast(_Msg, State) ->
	{noreply, State}.


handle_info(_Info, State) ->
    {noreply, State}.


terminate(_Reason, _State) ->
	ok.


code_change(_OldVsn, State, _Extra) ->
    {ok, State}.