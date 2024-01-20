#!/usr/bin/env bash

set -euo pipefail

plugins_dir="$XDG_CONFIG_HOME/zellij/plugins"

rm -rf "$plugins_dir"
mkdir -p "$plugins_dir"

curl -Lso "$plugins_dir/zjstatus.wasm" https://github.com/dj95/zjstatus/releases/download/v0.11.2/zjstatus.wasm
echo "Installed zjstatus.wasm"
