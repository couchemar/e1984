REBAR=./rebar

get-deps:
	@$(REBAR) get-deps

clean:
	@$(REBAR) clean

compile:
	@$(REBAR) compile

start: compile
	ERL_LIBS=apps:deps erl +A 4 +K true -name e1984@127.0.0.1 -setcookie e1984 -config apps/e1984/app.config -s e1984_app

ct: compile
	@$(REBAR) ct skip_deps=true

eunit: compile
	@$(REBAR) eunit skip_deps=true
