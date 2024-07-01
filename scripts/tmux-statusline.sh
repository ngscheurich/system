#!/usr/bin/env bash

set -euo pipefail

tmux set -g status-left "#[fg=$TMUX_COLOR_LEFT_FG,bg=$TMUX_COLOR_LEFT_BG]  #S "
tmux set -g status-style "fg=colour0,bg=$TMUX_COLOR_MAIN_BG"
tmux set -g window-status-style "fg=$TMUX_COLOR_MAIN_FG,bg=$TMUX_COLOR_MAIN_BG"
tmux set -g window-status-format "#[fg=$TMUX_COLOR_MAIN_FG,bg=$TMUX_COLOR_MAIN_BG] #I #[fg=$TMUX_COLOR_SEG_2_BG]│ #[fg=$TMUX_COLOR_MAIN_FG]#W "
tmux set -g window-status-current-format "#[fg=$TMUX_COLOR_ACTIVE_FG,bg=$TMUX_COLOR_ACTIVE_BG] #I #[fg=$TMUX_COLOR_SEG_2_BG]│ #[fg=$TMUX_COLOR_ACTIVE_FG]#W "
tmux set -g status-right "#[fg=$TMUX_COLOR_SEG_1_FG,bg=$TMUX_COLOR_SEG_1_BG] \
#(whoami)@#(/etc/system/scripts/tmux_hostname.sh) \
#[fg=$TMUX_COLOR_SEG_2_FG,bg=$TMUX_COLOR_SEG_2_BG] #{?window_zoomed_flag, ,}#{?client_prefix, , } \
#[fg=$TMUX_COLOR_SEG_3_FG,bg=$TMUX_COLOR_SEG_3_BG] #(/etc/system/scripts/tmux_datetime.sh) "
