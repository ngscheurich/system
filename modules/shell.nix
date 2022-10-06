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

  programs.fish = {
    enable = true;

    functions = {
      fish_greeting = "";
      gitignore = "curl -sL https://www.gitignore.io/api/$argv";
      tmuxdir = "tmux new-session -s $(pwd | string split '/')[-1]";
    };

    interactiveShellInit = ''
      fish_hybrid_key_bindings

      abbr -a cls clear
      abbr -a lg lazygit
      abbr -a love /Applications/love.app/Contents/MacOS/love
      abbr -a pico8 /Applications/PICO-8.app/Contents/MacOS/pico8
      abbr -a serve "python -m http.server"
      abbr -a ta "tmux attach"
      abbr -a vboxls "VBoxManage list runningvms"
      abbr -a vimdiff "nvim -d"

      alias l exa
      alias la "exa --long --all"
      alias ll "exa --long"
      alias ls exa
      alias lt "exa --tree"
      alias ta "tmux attach"
      alias tn tmuxdir
      alias weather "curl wttr.in"

      thefuck --alias | source

      # FZF colors
      set -gx FZF_DEFAULT_OPTS "--color \
      fg:7,\
      bg:0,\
      hl:8,\
      fg+:3,\
      bg+:0,\
      gutter:8,\
      hl+:1,\
      info:6,\
      prompt:2,\
      pointer:4,\
      marker:1,\
      spinner:5"
    '';

    shellInit = ''
      set -gx AWS_VAULT_KEYCHAIN_NAME login
      set -gx BROWSER open
      set -gx EDITOR nvim
      set -gx ERL_AFLAGS "-kernel shell_history enabled"
      set -gx JAVA_HOME /Library/Java/JavaVirtualMachines/temurin-11.jdk/Contents/Home
      set -gx KERL_CONFIGURE_OPTIONS --without-javac
      set -gx NVIM_LISTEN_ADDRESS /tmp/nvimsocket
      set -gx RANGER_DEVICONS_SEPARATOR "  "
      set -gx XDG_DATA_HOME $HOME/.local/share
      set -gx XDG_CONFIG_HOME $HOME/.config
      set -gx PLAYDATE_SDK_PATH $HOME/Developer/PlaydateSDK 

      fish_add_path /opt/homebrew/bin

      if string match -q -r '^N.*\.local$' "$(hostname)"
        set -gx ANDROID_SDK_ROOT $HOME/Library/Android/sdk

        fish_add_path $ANDROID_SDK_ROOT/emulator
        fish_add_path $ANDROID_SDK_ROOT/platform-tools
        fish_add_path $ANDROID_SDK_ROOT/tools
        fish_add_path $ANDROID_SDK_ROOT/tools/bin
      end

    '';

    plugins = [
      {
        name = "z";
        src = pkgs.fetchFromGitHub {
          owner = "jethrokuan";
          repo = "z";
          rev = "85f863f20f24faf675827fb00f3a4e15c7838d76";
          sha256 = "+FUBM7CodtZrYKqU542fQD+ZDGrd2438trKM0tIESs0=";
        };
      }
    ];
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
