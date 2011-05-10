
all: build

%.beam: %.erl
	erlc -o test/ $<

build:
	./rebar compile

clean:
	./rebar clean

check: build test/etap.beam
	prove test/*.t
