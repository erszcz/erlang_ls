#!/usr/bin/env bash

set -x

cp ./_build/default/bin/erlang_ls $(dirname $(asdf which erl))/
