.PHONY: all compile clean test

all: compile

compile:
	@rebar3 compile

clean:
	@rebar3 clean

test:
	@rebar3 eunit

dialyzer:
	@./rebar3 as dialyzer dialyzer
