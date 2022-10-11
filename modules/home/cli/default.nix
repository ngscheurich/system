# ----------------------------------------------------------
# Packages that help provide a rich, productive experience
# on the command-line
# ----------------------------------------------------------

{ lib, pkgs, ... }:

{
  programs.fish = {
    enable = true;

    shellAbbrs = {
      cls = "clear";
      lg = "lazygit";
      serve = "python -m http.server";
      ta = "tmux attach";
      vimdiff = "nvim -d";
    };

    
    shellAliases = {
      l = "exa";
      la = "exa --long --all";
      ll = "exa --long";
      ls = "exa";
      lt = "exa --tree";
      ta = "tmux attach";
      tn = "tmuxdir";
      weather = "curl wttr.in";
    };

    shellInit = builtins.readFile ./init.fish;
  };

  programs.atuin = {
    enable = true;
    settings = {
      auto_sync = false;
    };
  };

  programs.tmux = {
    enable = true;
    baseIndex = 1;
    clock24 = true;
    escapeTime = 1;
    keyMode = "vi";
    resizeAmount = 20;
    shortcut = "s";

    extraConfig = builtins.readFile ./tmux.conf;
  };

  programs.fzf = {
    enable = true;
    defaultOptions = [
      "--color fg:7,bg:0,hl:8,fg+:3,bg+:0,gutter:8,hl+:1,info:6,prompt:2,pointer:4,marker:1,spinner:5"
    ];
  };

  programs.bat = {
    enable = true;
    config.theme = "base16";
  };

  programs.starship = {
    enable = true;

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
    exa
    fasd
    fd
    figlet
    fzy
    htop
    httpie
    jq
    neofetch
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
