#!/usr/bin/env bash

set -euo pipefail

export TMUX_COLOR_MAIN_FG=colour8
export TMUX_COLOR_MAIN_BG=#e4dcd4

export TMUX_COLOR_LEFT_FG=colour7
export TMUX_COLOR_LEFT_BG=colour2

export TMUX_COLOR_ACTIVE_FG=colour0
export TMUX_COLOR_ACTIVE_BG=#f6f2ee

export TMUX_COLOR_SEG_1_FG=colour0
export TMUX_COLOR_SEG_1_BG=#e4dcd4

export TMUX_COLOR_SEG_2_FG=colour2
export TMUX_COLOR_SEG_2_BG=#e7d2be

export TMUX_COLOR_SEG_3_FG=colour0
export TMUX_COLOR_SEG_3_BG=colour7

/etc/system/scripts/tmux-statusline.sh
