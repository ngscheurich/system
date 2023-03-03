#!/bin/sh

export TMUX_COLOR_MAIN=#131a24

export TMUX_COLOR_LEFT_FG=colour0
export TMUX_COLOR_LEFT_BG=colour4

export TMUX_COLOR_ACTIVE_FG=colour7
export TMUX_COLOR_ACTIVE_BG=#192330

export TMUX_COLOR_SEG_1_FG=colour15
export TMUX_COLOR_SEG_1_BG=#2b3b51

export TMUX_COLOR_SEG_2_FG=colour4
export TMUX_COLOR_SEG_2_BG=#39506d

export TMUX_COLOR_SEG_3_FG=colour0
export TMUX_COLOR_SEG_3_BG=colour12

"$HOME/scripts/tmux-statusline.sh"
