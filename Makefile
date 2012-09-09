get-deps:
	@./rebar get-deps

clean:
	@./rebar clean

compile:
	@./rebar compile

start:
	make compile
	ERL_LIBS=apps:deps erl +A 4 +K true -config apps/e1984/app.config -eval "application:start(e1984)."
