#!/usr/bin/env bash

set -euo pipefail

export TMUX_COLOR_MAIN_FG=colour7
export TMUX_COLOR_MAIN_BG=colour0

export TMUX_COLOR_LEFT_FG=colour0
export TMUX_COLOR_LEFT_BG=colour2

export TMUX_COLOR_ACTIVE_FG=colour7
export TMUX_COLOR_ACTIVE_BG=#1f1f28

export TMUX_COLOR_SEG_1_FG=colour7
export TMUX_COLOR_SEG_1_BG=colour0

export TMUX_COLOR_SEG_2_FG=colour0
export TMUX_COLOR_SEG_2_BG=colour3

export TMUX_COLOR_SEG_3_FG=colour0
export TMUX_COLOR_SEG_3_BG=colour7

/etc/system/scripts/tmux-statusline.sh
