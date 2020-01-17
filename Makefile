.PHONY: all

all:
	@ echo "Building escript..."
	@ rebar3 escriptize

ci:
	rebar3 do compile, ct # ct sets -pa, need to restart for the res
	rebar3 do compile, proper --cover --constraint_tries 100, dialyzer, xref, cover, edoc

coveralls:
	rebar3 coveralls send
