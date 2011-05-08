
all: build

%.beam: %.erl
	erlc -o test/ $<

build:
	./rebar compile

#check: test/etap.beam test/util.beam
#	prove test/*.t
