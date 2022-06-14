#!/usr/bin/env bash

set -e
set -x

while rebar3 ct --suite apps/els_lsp/test/els_references_SUITE.erl
do :;
done
