#!/bin/sh

export TMUX_COLOR_MAIN=#20242d
export TMUX_COLOR_ACTIVE=#252a33
export TMUX_COLOR_SEG_1_FG=colour15
export TMUX_COLOR_SEG_1_BG=#3b4252
export TMUX_COLOR_SEG_2_FG=colour4
export TMUX_COLOR_SEG_2_BG=#545c7e
export TMUX_COLOR_SEG_3_FG=colour0
export TMUX_COLOR_SEG_3_BG=#81a1c1

"$HOME/scripts/tmux-statusline.sh"
