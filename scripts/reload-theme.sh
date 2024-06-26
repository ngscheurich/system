#!/usr/bin/env bash

set -euo pipefail

THEME_PATH="$HOME/.theme"

kitty @ set-colors --all --configured "$THEME_PATH/kitty.conf"
kitty @ load-config "$XDG_CONFIG_HOME/kitty/kitty.conf"

source "$THEME_PATH/shell.sh"
"$THEME_PATH/tmux.sh"
