#!/bin/sh

export TMUX_COLOR_MAIN=#282a2e
export TMUX_COLOR_ACTIVE=#1d1f21
export TMUX_COLOR_SEG_1_FG=colour15
export TMUX_COLOR_SEG_1_BG=#363a41
export TMUX_COLOR_SEG_2_FG=colour0
export TMUX_COLOR_SEG_2_BG=colour7
export TMUX_COLOR_SEG_3_FG=colour0
export TMUX_COLOR_SEG_3_BG=#c4c8c5

"$HOME/scripts/tmux-statusline.sh"
