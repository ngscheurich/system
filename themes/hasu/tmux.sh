#!/usr/bin/env bash

set -euo pipefail

export TMUX_COLOR_MAIN_FG=colour7
export TMUX_COLOR_MAIN_BG=#efe9dd

export TMUX_COLOR_LEFT_FG=#fbf7f0
export TMUX_COLOR_LEFT_BG=colour2

export TMUX_COLOR_ACTIVE_FG=colour7
export TMUX_COLOR_ACTIVE_BG=#fbf7f0

export TMUX_COLOR_SEG_1_FG=colour7
export TMUX_COLOR_SEG_1_BG=#e8e0d0

export TMUX_COLOR_SEG_2_FG=colour7
export TMUX_COLOR_SEG_2_BG=#dfd6c4

export TMUX_COLOR_SEG_3_FG=colour7
export TMUX_COLOR_SEG_3_BG=#d4c8b1

/etc/system/scripts/tmux-statusline.sh
