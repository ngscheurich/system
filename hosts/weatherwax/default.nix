{ pkgs, ... }:

{
  networking = {
    hostName = "weatherwax";
    computerName = "Weatherwax";
  };

  users = {
    knownUsers = [ "nick" ];

    users.nick = {
      home = "/Users/nick";
      description = "N. G. Scheurich";
      shell = pkgs.zsh;
      uid = 501;
    };
  };

  home-manager.users = {
    nick = {
      home.stateVersion = "22.11";

      imports = [
        ../../modules/home/code.nix
        ../../modules/home/crypt.nix
        ../../modules/home/devel.nix
        ../../modules/home/devops.nix
        ../../modules/home/git.nix
        ../../modules/home/prose.nix
        ../../modules/home/shell.nix
        ../../modules/home/tasks.nix
        ../../modules/home/vim.nix
      ];
    };
  };

  homebrew = {
    brews = [
      "postgresql@14"
      "timescaledb"
      "timescaledb-tools"
    ];

    casks = [
      "affinity-designer"
      "affinity-photo"
      "affinity-publisher"
      "epic-games"
      "godot"
      "playdate-simulator"
      "protonvpn"
      "steam"
    ];
  };
}
