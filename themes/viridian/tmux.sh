#!/usr/bin/env bash

set -euo pipefail

export TMUX_COLOR_MAIN_FG=colour7
export TMUX_COLOR_MAIN_BG=#3e5a5d

export TMUX_COLOR_LEFT_FG=#0f3b3a
export TMUX_COLOR_LEFT_BG=colour2

export TMUX_COLOR_ACTIVE_FG=colour7
export TMUX_COLOR_ACTIVE_BG=#0f3b3a

export TMUX_COLOR_SEG_1_FG=colour7
export TMUX_COLOR_SEG_1_BG=#3e5a5d

export TMUX_COLOR_SEG_2_FG=colour3
export TMUX_COLOR_SEG_2_BG=#566f71

export TMUX_COLOR_SEG_3_FG=#0f3b3a
export TMUX_COLOR_SEG_3_BG=#b7c1c2

/etc/system/scripts/tmux-statusline.sh
