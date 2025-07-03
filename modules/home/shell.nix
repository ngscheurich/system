# ----------------------------------------------------------
# Packages that help provide a rich, productive experience
# on the command-line
# ----------------------------------------------------------

{ lib, pkgs, ... }:

let
  shellAliases = {
    cls = "clear";
    G = "lazygit";
    g = "git";
    gP = "git push";
    ga = "git add";
    gci = "git commit";
    gcl = "git clone";
    gco = "git checkout";
    gsw = "git switch";
    gg = "git log --all --decorate --oneline --graph";
    gl = "git log --oneline";
    gp = "git pull";
    gr = "git rebase";
    gs = "git status";
    l = "eza";
    ls = "eza";
    la = "eza --all --long";
    ll = "eza --long";
    lt = "eza --tree";
    n = "nvim";
    ns = "nvim -S";
    serve = "python -m http.server";
    vimdiff = "nvim -d";
    weather = "curl wttr.in";
    zj = "zellij";
    zp = "zellij --layout project.kdl --session (basename $PWD)";
    zr = "zellij run -- ";
    tx = "tmux";
  };

  shellAbbrs = shellAliases;
in
{
  programs.bash = {
    enable = true;
    inherit shellAliases;

    bashrcExtra = ''
          [ -n "$EAT_SHELL_INTEGRATION_DIR" ] && \
      source "$EAT_SHELL_INTEGRATION_DIR/bash"
    '';
  };

  programs.zsh = {
    enable = true;
    autosuggestion.enable = true;
    enableCompletion = true;
    defaultKeymap = "viins";
    syntaxHighlighting.enable = true;

    inherit shellAliases;

    envExtra = builtins.readFile ./files/.zshenv;
    loginExtra = builtins.readFile ./files/.zlogin;
  };

  programs.fish = {
    enable = true;

    functions = {
      fish_greeting = "";
    };

    inherit shellAbbrs;

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
      {
        name = "plugin-bang-bang";
        src = pkgs.fetchFromGitHub {
          owner = "oh-my-fish";
          repo = "plugin-bang-bang";
          rev = "816c66df34e1cb94a476fa6418d46206ef84e8d3";
          sha256 = "0dx4z0mmrwfkg8qh1yis75vwf69ng51m3icsiiw7k2cwc02mg76z";
        };
      }
    ];

    shellInit = builtins.readFile ./files/init.fish;
  };

  programs.starship = {
    enable = true;
    enableBashIntegration = false;

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
        "$erlang"
        "$cmd_duration"
        "$line_break"
        "$jobs"
        "$character"
      ];

      character = {
        vimcmd_symbol = "[🞈](bold blue)";
      };

      directory = {
        read_only = "";
      };

      fill = {
        symbol = " ";
      };

      git_branch = {
        symbol = " ";
        style = "purple";
        truncation_length = 32;
      };
      git_commit = {
        tag_symbol = " ";
      };
      git_status = {
        style = "bold purple";
      };

      nix_shell = {
        symbol = "";
        format = "[$symbol nix]($style) ";
        style = "blue";
      };
    };
  };

  programs.direnv.enable = true;
  programs.direnv.nix-direnv.enable = true;

  programs.fzf = {
    enable = true;
  };

  programs.bat = {
    enable = true;
    config.theme = "ansi";
  };

  programs.fastfetch = {
    enable = true;
    settings = {
      modules = [
        "title"
        "separator"
        "os"
        "host"
        "kernel"
        "break"
        "cpu"
        "memory"
        "uptime"
        "loadavg"
        "processes"
        "disk"
        "break"
        "localip"
        "publicip"
        "netio"
        "break"
        "packages"
        "terminal"
        "shell"
        "editor"
      ];
    };
  };

  home.packages = with pkgs; [
    # File management
    eza
    fasd
    tree
    yazi

    # Find/replace
    ack
    fd
    fzy
    ripgrep
    sd

    # Monitoring
    bottom
    htop

    # Networking
    httpie
    ngrok
    websocat
    wget

    # Multiplexing
    tmux
    tmuxp

    # Generative AI
    aichat
    vectorcode

    # Miscellaneous
    asciinema
    figlet
    fswatch
    gnused
    graph-easy
    gum
    imagemagick
    jq
    pay-respects
    slides
    tldr
  ];
}
