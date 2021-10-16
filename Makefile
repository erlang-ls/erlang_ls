.PHONY: all

all:
	@ echo "Building escript..."
	@ rebar3 escriptize
	@ rebar3 as dap escriptize

install: all
	@ echo "Installing escript..."
	@ cp _build/default/bin/erlang_ls /usr/local/bin
	@ cp _build/dap/bin/els_dap /usr/local/bin

.PHONY: clean
clean:
	@rm -rf _build

$HOME/.dialyzer_plt:
	dialyzer --build_plt --apps erts kernel stdlib

ci: $HOME/.dialyzer_plt
	rebar3 do compile, ct, proper --cover --constraint_tries 100, dialyzer, xref, cover, edoc

coveralls:
	rebar3 coveralls send
