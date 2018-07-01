.PHONY: eqc
eqc: compile
	cd test; \
	erl -noshell -eval "make:all([debug_info, nowarn_missing_spec_all, {d, 'EQC_TESTING'}, {parse_transform, eqc_cover}])" -s init stop

.PHONY: compile
compile:
	rebar3 as eqc compile
