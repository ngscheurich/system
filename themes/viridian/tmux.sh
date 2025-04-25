#!/usr/bin/env bash

set -euo pipefail

export TMUX_COLOR_MAIN_FG=colour7
export TMUX_COLOR_MAIN_BG=#2c6463

export TMUX_COLOR_LEFT_FG=#0f3b3a
export TMUX_COLOR_LEFT_BG=colour2

export TMUX_COLOR_ACTIVE_FG=colour7
export TMUX_COLOR_ACTIVE_BG=#0f3b3a

export TMUX_COLOR_SEG_1_FG=colour7
export TMUX_COLOR_SEG_1_BG=#2c6463

export TMUX_COLOR_SEG_2_FG=colour4
export TMUX_COLOR_SEG_2_BG=#155352

export TMUX_COLOR_SEG_3_FG=#0f3b3a
export TMUX_COLOR_SEG_3_BG=#b1c9c3

/etc/system/scripts/tmux-statusline.sh
