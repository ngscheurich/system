{ pkgs, ... }:

{
  networking = {
    hostName = "joulie";
    computerName = "Joulie";
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
    knownUsers = [ "nscheurich" ];

    users.nscheurich = {
      home = "/Users/nscheurich";
      description = "N. G. Scheurich";
      shell = pkgs.fish;
      uid = 317732888;
    };
  };

  home-manager.backupFileExtension = "backup";

  home-manager.users = {
    nscheurich = {
      home.stateVersion = "22.11";

      imports = [
        ../../modules/home/code.nix
        ../../modules/home/crypt.nix
        ../../modules/home/devel.nix
        ../../modules/home/devops.nix
        ../../modules/home/git.nix
        ../../modules/home/mail.nix
        ../../modules/home/prose.nix
        ../../modules/home/shell.nix
        ../../modules/home/tasks.nix
        ../../modules/home/vim.nix
      ];
    };
  };

  homebrew = {
    brews = [
      "xcode-build-server"
    ];

    casks = [
      "steam"
    ];
  };
}
