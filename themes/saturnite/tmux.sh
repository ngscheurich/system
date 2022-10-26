#!/bin/sh

export TMUX_COLOR_MAIN=#262626
export TMUX_COLOR_ACTIVE=#1c1c1c
export TMUX_COLOR_SEG_1_FG=colour15
export TMUX_COLOR_SEG_1_BG=#484848
export TMUX_COLOR_SEG_2_FG=colour0
export TMUX_COLOR_SEG_2_BG=colour7
export TMUX_COLOR_SEG_3_FG=colour0
export TMUX_COLOR_SEG_3_BG=#9c9c9c

"$HOME/scripts/tmux-statusline.sh"
