#!/usr/bin/env bash

set -euo pipefail

export TMUX_COLOR_MAIN_FG=colour7
export TMUX_COLOR_MAIN_BG=#e7dba0

export TMUX_COLOR_LEFT_FG=colour0
export TMUX_COLOR_LEFT_BG=colour2

export TMUX_COLOR_ACTIVE_FG=colour7
export TMUX_COLOR_ACTIVE_BG=#f2ecbc

export TMUX_COLOR_SEG_1_FG=colour0
export TMUX_COLOR_SEG_1_BG=#e7dba0

export TMUX_COLOR_SEG_2_FG=#e7dba0
export TMUX_COLOR_SEG_2_BG=colour3

export TMUX_COLOR_SEG_3_FG=#e7dba0
export TMUX_COLOR_SEG_3_BG=colour11

/etc/system/scripts/tmux-statusline.sh
