REBAR?=rebar3

all: compile

compile:
	@$(REBAR) compile

clean:
	@$(REBAR) clean
	@rm -rf doc

test:
	@$(REBAR) ct -v

dialyzer:
	@$(REBAR) as dialyzer dialyzer

xref:
	@$(REBAR) xref

ifeq (, $(shell which elp))
eqwalizer:
else
eqwalizer:
	@elp eqwalize-all
endif

checks: xref dialyzer eqwalizer

hex-build:
	@$(REBAR) hex build

.PHONY: all compile clean test dialyzer xref eqwalizer checks
