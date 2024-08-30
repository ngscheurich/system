#!/usr/bin/env bash

# See https://blog.meain.io/2020/tmux-flating-scratch-terminal/

set -euo pipefail

if [ "$(tmux display-message -pF "#{session_name}")" = "popup" ]; then
	tmux detach-client
else
	tmux popup -w80% -h80% -E "tmux attach -t popup || tmux new -s popup"
fi
