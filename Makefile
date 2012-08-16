REBAR = ./rebar

all: get-deps compile run

get-deps:
	@./rebar get-deps

compile:
	@$(REBAR) compile

run:
	@erl -args_file 'vm.args' -pa ebin -pa deps/*/ebin

daemon:
	@erl -args_file 'vm.args' -pa ebin -pa deps/*/ebin -detached

remsh:
	@erl -sname remsh -setcookie CzFG345gRfRgRYp45f86 -remsh prime@$(shell hostname -s)

pull:
	@git pull

progress:
	@./priv/progress.escript prime@$(shell hostname -s)

save:
	@./priv/save.escript prime@$(shell hostname -s)

stop:
	@./priv/stop.escript prime@$(shell hostname -s)

test: compile
	@$(REBAR) eunit skip_deps=true
	
clean:
	@$(REBAR) clean
	rm -f ./erl_crash.dump ./src/erl_crash.dump
	rm -f ./log/*.log