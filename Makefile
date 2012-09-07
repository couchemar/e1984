get-deps:
	@./rebar get-deps

clean:
	@./rebar clean

compile:
	@./rebar compile

start:
	make compile
	@erl +K true +A 4 -pa ebin/ -pa deps/*/ebin/ -config src/app.config -s e1984_app
