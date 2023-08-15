REBAR?=rebar3

all: compile

compile:
	@$(REBAR) compile

clean:
	@$(REBAR) clean
	@rm -rf doc

test:
	@$(REBAR) ct -v -c
	@$(REBAR) cover -v

dialyzer:
	@$(REBAR) as dialyzer dialyzer

xref:
	@$(REBAR) xref

checks: xref dialyzer

hex-build:
	@$(REBAR) hex build

.PHONY: all compile clean test dialyzer xref checks
