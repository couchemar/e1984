get-deps:
	@./rebar get-deps

clean:
	@./rebar clean

compile:
	@./rebar compile

start:
	make compile
	@erl +A 4 -pa ebin/ -pa deps/*/ebin/ -config src/app.config -eval "application:start(e1984)."
