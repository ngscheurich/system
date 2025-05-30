#!/usr/bin/env bash

set -euo pipefail

if [[ $# -eq 1 ]]; then
  ls ${XDG_RUNTIME_DIR:-${TMPDIR}nvim.${USER}}/*/nvim.*.0 | while read -r server; do
    nvim --server "$server" --remote-send "$1"
  done
else
  exit 1
fi
