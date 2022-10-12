# ----------------------------------------------------------
# The Git version control system, plus configuration and
# supplementary packages
# ----------------------------------------------------------

{ pkgs, ... }:

{
  programs.git = {
    enable = true;

    userName = "N. G. Scheurich";
    userEmail = "nick@scheurich.me";

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
      "*.sql"
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
      "NOTES.md"

      # Vim
      ".netrwhist"
      "Session.vim"

      # Secrets
      ".env"
      ".env.staging"
      ".env.production"
      ".envrc"
      ".http.env"

      # Elixir
      ".elixir_ls"
      "__lab__.ex"

      # Projections
      ".projections.json"

      # Local Neovim config
      ".lnvim.*"

      # Miscellaneous project files
      "requests.http"
    ];

    signing = {
      signByDefault = true;
      key = "CBA4974EC069B4891D84502ECBEABCE653DD30E5";
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