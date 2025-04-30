#!/usr/bin/env bash

set -euo pipefail

export TMUX_COLOR_MAIN_FG=colour7
export TMUX_COLOR_MAIN_BG=#363a41

export TMUX_COLOR_LEFT_FG=#1d1f21
export TMUX_COLOR_LEFT_BG=colour2

export TMUX_COLOR_ACTIVE_FG=colour7
export TMUX_COLOR_ACTIVE_BG=#1d1f21

export TMUX_COLOR_SEG_1_FG=colour7
export TMUX_COLOR_SEG_1_BG=#363a41

export TMUX_COLOR_SEG_2_FG=colour4
export TMUX_COLOR_SEG_2_BG=#282a2e

export TMUX_COLOR_SEG_3_FG=#1d1f21
export TMUX_COLOR_SEG_3_BG=#c5c8c6

/etc/system/scripts/tmux-statusline.sh
