#!/usr/bin/env bash

set -e
set -x

while rebar3 ct --suite apps/els_lsp/test/els_references_SUITE.erl --case ignore_open_watched_file_added
do :;
done
