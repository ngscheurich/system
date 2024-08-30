#!/usr/bin/env bash

set -euo pipefail

export TMUX_COLOR_MAIN_FG=colour7
export TMUX_COLOR_MAIN_BG=#131a24

export TMUX_COLOR_LEFT_FG=colour0
export TMUX_COLOR_LEFT_BG=colour2

export TMUX_COLOR_ACTIVE_FG=colour7
export TMUX_COLOR_ACTIVE_BG=#192330

export TMUX_COLOR_SEG_1_FG=colour7
export TMUX_COLOR_SEG_1_BG=#131a24

export TMUX_COLOR_SEG_2_FG=colour2
export TMUX_COLOR_SEG_2_BG=#2b3b51

export TMUX_COLOR_SEG_3_FG=colour0
export TMUX_COLOR_SEG_3_BG=colour12

/etc/system/scripts/tmux-statusline.sh
