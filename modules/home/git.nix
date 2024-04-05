# ----------------------------------------------------------
# The Git version control system, plus configuration and
# supplementary packages
# ----------------------------------------------------------

{ pkgs, ... }:

{
  programs.git = {
    enable = true;

    userName = "N. G. Scheurich";
    userEmail = "nick@scheurich.haus";

    aliases = {
      amend = "commit --amend --message";
      br = "branch";
      ci = "commit";
      co = "checkout";
      fixup = "commit --fixup";
      pub = "push --set-upstream origin HEAD";
      rb = "rebase";
      rba = "rebase --abort";
      rbc = "rebase --continue";
      st = "status";
      unstage = "reset --soft HEAD^";
    };

    ignores = [
      # Based on https://gist.github.com/octocat/9257657

      # Compiled source
      "*.com"
      "*.class"
      "*.dll"
      "*.exe"
      "*.o"
      "*.so"

      # Packages
      "*.7z"
      "*.dmg"
      "*.gz"
      "*.iso"
      "*.jar"
      "*.rar"
      "*.tar"
      "*.zip"

      # Logs and databases
      "*.log"
      "*.sqlite"

      # OS generated files
      ".DS_Store"
      ".DS_Store?"
      "._*"
      ".Spotlight-V100"
      ".Trashes"
      "ehthumbs.db"
      "Thumbs.db"

      # Temporary files
      "*.swp"
      "*.swo"
      "*~"

      # Tags
      "tags"

      # Project notes
      "TODO.md"
      "TODO.org"
      "NOTES.md"
      "NOTES.org"
      "QUERIES.sql"

      # Vim
      ".netrwhist"
      "Session.vim"

      # Emacs
      ".dir-locals.el"

      # Secrets
      ".env"
      ".env.staging"
      ".env.production"

      # Elixir
      ".elixir_ls"
      "__scratch__.ex"

      # Projections
      ".projections.json"

      # Local Neovim config
      ".lnvim.*"

      # Miscellaneous project files
      "requests.http"
      ".sqllsrc.json"

      # direnv caches
      ".direnv/"

      # Local tmux configs
      ".tmux.local.conf"

      # Project-specific Zellij layouts
      "project.kdl"
      "layout.kdl"
    ];

    signing = {
      signByDefault = true;
      key = "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAINcR6+hD9GfRhh0pny+dUOb+NzMlQdSYuyhQ4YkS4eW1";
    };

    extraConfig = {
      color = {
        diff = {
          commit = "yellow bold";
          frag = "magenta bold";
          func = "146 bold";
          meta = "11";
          new = "green bold";
          old = "red bold";
          whitespace = "red reverse";
        };
        diff-highlight = {
          newHighlight = "green bold 22";
          newNormal = "green bold";
          oldHighlight = "red bold 52";
          oldNormal = "red bold";
        };
        ui = true;
      };

      core = {
        editor = "nvim";
        pager = "diff-so-fancy | less --tabs=4 -RFX";
        ignorecase = "true";
      };

      diff = {
        submodule = "log";
        tool = "vimdiff";
      };

      fetch.prune = true;

      filter.lfs = {
        smudge = "git-lfs smudge -- %f";
        process = "git-lfs filter-process";
        required = "true";
        clean = "git-lfs clean -- %f";
      };

      github.user = "ngscheurich";

      init.defaultBranch = "main";

      interactive.diffFilter = "diff-so-fancy --patch";

      merge = {
        ff = "only";
        tool = "vimdiff";
        conflictstyle = "diff3";
        prompt = "false";
      };

      mergetool.vimdiff.cmd = ''
        nvim -d $LOCAL $BASE $REMOTE $MERGED -c "$wincmd w" -c "wincmd J"
      '';

      pull.ff = "only";

      rebase.autosquash = true;

      url = {
        "https://github" = {
          insteadOf = "git://github";
        };
      };

      gpg = {
        format = "ssh";
        ssh = {
          program = "/Applications/1Password.app/Contents/MacOS/op-ssh-sign";
        };
      };
    };
  };

  programs.gh = {
    enable = true;

    settings = {
      git_protocol = "ssh";
      prompt = "enabled";
      aliases = {
        co = "pr checkout";
        pv = "pr view";
      };
    };
  };

  programs.lazygit = {
    enable = true;
  };

  home.packages = with pkgs; [
    diff-so-fancy
    gist
    git-lfs
  ];
}
