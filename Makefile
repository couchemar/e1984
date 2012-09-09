get-deps:
	@./rebar get-deps

clean:
	@./rebar clean

compile:
	@./rebar compile

start:
	make compile
	ERL_LIBS=apps:deps erl +A 4 +K true -name e1984@127.0.0.1 -setcookie e1984 -config apps/e1984/app.config -s e1984_app
