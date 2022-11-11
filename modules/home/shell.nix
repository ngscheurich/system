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

    shellInit = builtins.readFile ./files/init.fish;

    functions = {
      fish_greeting = "function fish_greeting; end";
    };
  };

  programs.direnv.enable = true;
  programs.direnv.nix-direnv.enable = true;

  programs.zoxide.enable = true;

  programs.atuin = {
    enable = false;
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

    extraConfig = builtins.readFile ./files/tmux.conf;
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
    starship
    thefuck
    tldr
    tree
    watch
    watchman
    wget
  ];
}
