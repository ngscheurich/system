# ----------------------------------------------------------
# Packages that help provide a rich, productive experience
# on the command-line
# ----------------------------------------------------------

{ lib, pkgs, ... }:

{
  programs.direnv.enable = true;
  programs.direnv.nix-direnv.enable = true;

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
