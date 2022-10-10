# ----------------------------------------------------------
# Packages that help provide a rich, productive experience
# on the command line
# ----------------------------------------------------------

{ lib, pkgs, ... }:

{
  programs.bash = {
    enable = true;
    initExtra = ''
      PATH=$HOME/.asdf/shims:$PATH
    '';
  };

  programs.tmux = {
    enable = true;
    baseIndex = 1;
    clock24 = true;
    escapeTime = 1;
    keyMode = "vi";
    resizeAmount = 20;
    shortcut = "s";

    extraConfig = ''
      # Remap split commands
      bind | split-window -h
      bind - split-window -v
      unbind '"'
      unbind %

      # Use vi-like keys
      setw -g mode-keys vi
      bind h select-pane -L
      bind j select-pane -D
      bind k select-pane -U
      bind l select-pane -R
      bind Right resize-pane -R 20
      bind Down  resize-pane -D 10
      bind Up    resize-pane -U 10
      bind Left  resize-pane -L 20

      # Select window by number
      bind-key -n M-1 select-window -t 1
      bind-key -n M-2 select-window -t 2
      bind-key -n M-3 select-window -t 3
      bind-key -n M-4 select-window -t 4
      bind-key -n M-5 select-window -t 5
      bind-key -n M-6 select-window -t 6
      bind-key -n M-7 select-window -t 7
      bind-key -n M-8 select-window -t 8
      bind-key -n M-9 select-window -t 9

      # Clicking and scrolling is okay sometimes
      set -g mouse on

      # Set terminal emulator window title to session / process
      set-option -g set-titles on
      set-option -g set-titles-string "#S / #W"

      # True Color support
      set-option -g default-terminal "screen-256color"
      set-option -ga terminal-overrides ",xterm-256color:Tc"

      # Use Fish
      set -g default-shell /run/current-system/sw/bin/fish

      # Configure status bar
      WINDOW=" #I | #W "

      set-option -g status-position top

      set  -g status "on"
      set  -g status-style "fg=colour0,bg=colour8"
      set  -g status-left-length "100"
      set  -g status-left "#[fg=colour0,bg=colour4] #S "
      set  -g status-right-length "100"
      set  -g status-right "#[fg=colour15,bg=colour8] #(whoami)@#(hostname)  #[fg=colour0,bg=colour7] #() #[fg=colour0,bg=colour15] #() "
      set  -g message-style "fg=colour0,bg=colour11"
      set  -g message-command-style "fg=colour0,bg=colour2"
      setw -g window-status-style "fg=colour15,bg=colour8"
      setw -g window-status-format $WINDOW
      setw -g window-status-current-format "#[fg=colour15,bg=colour0]$WINDOW"
    '';
  };

  programs.fzf = {
    enable = true;
    enableZshIntegration = true;
  };

  programs.bat = {
    enable = true;
    config.theme = "base16";
  };

  programs.starship = {
    enable = true;

    enableBashIntegration = false;

    settings = {
      format = lib.concatStrings [
        "$username"
        "$hostname"
        "$directory"
        "$sudo"
        "$cmd_duration"
        "$git_branch"
        "$git_commit"
        "$git_state"
        "$git_metrics"
        "$git_status"
        "$fill"
        "$time"
        "$battery"
        "$line_break"
        "$jobs"
        "$status"
        "$character"
      ];

      time = {
        disabled = false;
        format = "[$time]($style) ";
        style = "bright-black";
        time_format = "%R";
      };
    };
  };

  home.packages = with pkgs; [
    ack
    asciinema
    ddgr
    direnv
    exa
    fasd
    fd
    figlet
    fish
    fzy
    htop
    httpie
    jq
    ngrok
    ranger
    ripgrep
    slides
    thefuck
    tldr
    tree
    watch
    watchman
    wget
  ];
}