#!/bin/bash
./rebar clean
./rebar compile
erl -pa ebin/ -pa deps/*/ebin/ -eval "application:start(e1984)."
