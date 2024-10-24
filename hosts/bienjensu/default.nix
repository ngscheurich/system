{ pkgs, ... }:

{
  networking = {
    hostName = "bienjensu";
    computerName = "Bienjensu";
  };

  nix.settings = {
    trusted-substituters = [
      "https://cache.nixos.org"
      "https://nix-community.cachix.org"
    ];

    trusted-public-keys = [
      "cache.nixos.org-1:6NCHdD59X431o0gWypbMrAURkbJ16ZPMQFGspcDShjY="
      "nix-community.cachix.org-1:mB9FSh9qf2dCimDSUo8Zy7bkq5CX+/rkCWyvRCYg3Fs="
    ];

    substituters = [
      "https://cache.nixos.org"
      "https://nix-community.cachix.org"
    ];
  };

  users = {
    knownUsers = [ "nick" ];

    users.nick = {
      home = "/Users/nick";
      description = "N. G. Scheurich";
      shell = pkgs.fish;
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
    casks = [
      "affinity-designer"
      "affinity-photo"
      "affinity-publisher"
      "blender"
      "epic-games"
      "godot"
      "playdate-simulator"
      "protonvpn"
      "steam"
    ];
  };
}
