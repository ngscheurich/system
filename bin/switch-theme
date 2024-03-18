#!/usr/bin/env bash

set -euo pipefail

readonly THEME_PATH="$HOME/.theme"
readonly THEMES_PATH="/etc/system/themes"

if [ -d "$THEME_PATH" ]; then
	rm "$THEME_PATH"
fi

if [ $# -eq 0 ]; then
	# shellcheck disable=SC2012
	ln -s "$THEMES_PATH/$(ls $THEMES_PATH | gum filter --limit 1)" "$THEME_PATH"
else
	ln -s "$THEMES_PATH/$1" "$THEME_PATH"
fi

reload-theme
