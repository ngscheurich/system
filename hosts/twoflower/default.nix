{ pkgs, ... }:

{
  imports = [
    ../../modules/home
    ./configuration.nix
    ./hardware.nix
  ];

  home-manager = {
    users.nick = {
      imports = [
        ../../home/modules/cli
        ../../home/modules/git.nix
        ../../home/modules/vim.nix
      ];
    };
  };
}
