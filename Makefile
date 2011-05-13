
all: build

build:
	@./rebar compile
	@./rebar dialyze

clean:
	@./rebar clean

erl_clean:
	@rm -fv ebin/*

check: build test/etap.beam
	@prove test/*.t
