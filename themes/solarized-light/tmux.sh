#!/usr/bin/env bash

set -euo pipefail

export TMUX_COLOR_MAIN_FG=colour0
export TMUX_COLOR_MAIN_BG=#eee8d5

export TMUX_COLOR_LEFT_FG=colour7
export TMUX_COLOR_LEFT_BG=colour2

export TMUX_COLOR_ACTIVE_FG=colour0
export TMUX_COLOR_ACTIVE_BG=colour15

export TMUX_COLOR_SEG_1_FG=colour0
export TMUX_COLOR_SEG_1_BG=#eee8d5

export TMUX_COLOR_SEG_2_FG=colour15
export TMUX_COLOR_SEG_2_BG=colour8

export TMUX_COLOR_SEG_3_FG=colour7
export TMUX_COLOR_SEG_3_BG=colour10

/etc/system/scripts/tmux-statusline.sh
