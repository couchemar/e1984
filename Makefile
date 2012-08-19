get-deps:
	@./rebar get-deps

clean:
	@./rebar clean

compile:
	@./rebar compile

start:
	make compile
	@erl -pa ebin/ -pa deps/*/ebin/ -eval "application:start(e1984)."
