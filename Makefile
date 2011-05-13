
all: build

build:
	@./rebar compile

clean:
	@./rebar clean

erl_clean:
	@rm -fv ebin/*



check: build test/etap.beam
	@if [ ! -e ~/.*dialyzer_plt ] ; \
	then \
		echo "You might need to run ./rebar build-plt'" ; \
	fi
	@./rebar dialyze
	@prove test/*.t

test/%.beam: test/%.erl
	erlc +debug_info -o test/ $<
