#!/usr/bin/env bash

set -euo pipefail

cd "$SCRIPTS_DIR"
mix run --no-mix-exs nvim-plugins.exs
