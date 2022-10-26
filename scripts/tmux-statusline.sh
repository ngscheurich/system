#!/bin/sh

set -e

SCRIPTS=$HOME/scripts

tmux set -g status-style "fg=colour0,bg=$TMUX_COLOR_MAIN"
tmux set -g window-status-style "fg=colour7,bg=$TMUX_COLOR_MAIN"
tmux set -g window-status-current-format "#[fg=colour15,bg=$TMUX_COLOR_ACTIVE]$WINDOW"
tmux set -g status-right "#[fg=$TMUX_COLOR_SEG_1_FG,bg=$TMUX_COLOR_SEG_1_BG] \
#(whoami)@#($SCRIPTS/tmux-hostname.sh) \
#[fg=$TMUX_COLOR_SEG_2_FG,bg=$TMUX_COLOR_SEG_2_BG] #{?window_zoomed_flag, ,}#{?client_prefix, , } \
#[fg=$TMUX_COLOR_SEG_3_FG,bg=$TMUX_COLOR_SEG_3_BG] #($SCRIPTS/tmux-datetime.sh) "
