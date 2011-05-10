
all: build

%.beam: %.erl
	erlc -o test/ $<

build:
	./rebar compile

clean:
	./rebar clean

check: test/etap.beam
	prove test/*.t
