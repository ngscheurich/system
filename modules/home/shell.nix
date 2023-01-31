# ----------------------------------------------------------
# Packages that help provide a rich, productive experience
# on the command-line
# ----------------------------------------------------------

{ lib, pkgs, ... }:

{
  programs.fish = {
    enable = true;

    functions = {
      fish_greeting = "";
    };

    shellAbbrs = {
      cls = "clear";
      g = "git";
      la = "exa --all --long";
      lg = "lazygit";
      l = "exa"; 
      ll = "exa --long";
      ls = "exa";
      lt = "exa --tree";
      serve = "python -m http.server";
      ta = "tmux attach";
      tmux = "tmucks";
      vimdiff = "nvim -d";
      weather = "curl wttr.in";
    };

    plugins = [
      {
        name = "z";
        src = pkgs.fetchFromGitHub {
          owner = "jethrokuan";
          repo = "z";
          rev = "ddeb28a7b6a1f0ec6dae40c636e5ca4908ad160a";
          sha256 = "1kaa0k9d535jnvy8vnyxd869jgs0ky6yg55ac1mxcxm8n0rh2mgq";
        };
      }
    ];

    shellInit = builtins.readFile ./files/init.fish;
  };

  programs.starship = {
    enable = true;

    settings = {
      format = lib.concatStrings [
        "$username"
        "$hostname"
        "$localip"
        "$directory"
        "$nix_shell"
        "$git_branch"
        "$git_commit"
        "$git_state"
        "$git_metrics"
        "$git_status"
        "$fill"
        "$cmd_duration"
        "$line_break"
        "$jobs"
        "$character"
      ];

      character = { vimcmd_symbol = "[🅽 ](bold blue)"; };

      directory = { read_only = ""; };

      fill = { symbol = " "; };

      git_branch = {
        symbol = " ";
        style = "red";
        truncation_length = 32;
      };
      git_commit = { tag_symbol = " "; };
      git_status = { style = "bold bright-red"; };

      nix_shell = {
        symbol = "";
        format = "[$symbol nix]($style) ";
        style = "blue";
      };
    };
  };

  programs.direnv.enable = true;
  programs.direnv.nix-direnv.enable = true;

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
    graph-easy
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
    tmux
    tree
    watch
    watchman
    wget
  ];
}
