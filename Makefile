.PHONY: all

all:
	rebar3 do compile, ct, proper --cover --constraint_tries 100, dialyzer, xref, cover

coveralls:
	rebar3 coveralls send
