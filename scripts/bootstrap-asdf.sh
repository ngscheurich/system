#!/bin/bash

set -e

LANGS=(elixir erlang lua nodejs python ruby rust)

for lang in "${LANGS[@]}"; do
  asdf plugin add "$lang"
done

VERSIONS=("elixir main-otp-25" "erlang 25.0.3" "lua 5.3.6" "nodejs 16.13.2" "python 3.10.1" "ruby 3.1.0" "rust 1.61.0")

for version in "${VERSIONS[@]}"; do
  # shellcheck disable=SC2086
  asdf install $version
done
