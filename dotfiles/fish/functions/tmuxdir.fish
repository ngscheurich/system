function tmuxdir
    tmux new-session -s $(pwd | string split '/')[-1]
end

