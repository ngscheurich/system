#!/bin/sh

export TMUX_COLOR_MAIN=#e4dcd4

export TMUX_COLOR_LEFT_FG=colour7
export TMUX_COLOR_LEFT_BG=colour4

export TMUX_COLOR_ACTIVE_FG=colour0
export TMUX_COLOR_ACTIVE_BG=#f6f2ee

export TMUX_COLOR_SEG_1_FG=colour8
export TMUX_COLOR_SEG_1_BG=#dbd1dd

export TMUX_COLOR_SEG_2_FG=colour7
export TMUX_COLOR_SEG_2_BG=#aab0ad

export TMUX_COLOR_SEG_3_FG=colour0
export TMUX_COLOR_SEG_3_BG=#e7d2be

"$HOME/scripts/tmux-statusline.sh"
