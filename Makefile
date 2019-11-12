.PHONY: all

all:
	@ echo "Building escript..."
	@ rebar3 escriptize

ci:
	rebar3 do compile, ct, proper --cover --constraint_tries 100, dialyzer, xref, cover

coveralls:
	rebar3 coveralls send
