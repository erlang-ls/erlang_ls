.PHONY: all

all:
	@ echo "Building escript..."
	@ rebar3 escriptize

install: all
	@ echo "Installing escript..."
	@ cp _build/default/bin/erlang_ls /usr/local/bin

ci:
	rebar3 do compile, ct, proper --cover --constraint_tries 100, dialyzer, xref, cover, edoc

coveralls:
	rebar3 coveralls send
