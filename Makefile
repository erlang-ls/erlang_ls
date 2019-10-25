.PHONY: all

all:
	rebar3 do compile, ct, proper --cover, dialyzer, xref, cover

coveralls:
	rebar3 coveralls send
