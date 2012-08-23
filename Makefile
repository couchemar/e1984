get-deps:
	@./rebar get-deps

clean:
	@./rebar clean

compile:
	@./rebar compile

start:
	make compile
	@erl -pa ebin/ -pa deps/*/ebin/ -config src/app.config -eval "application:start(e1984)."
