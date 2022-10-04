{ pkgs, ... }:

{
  imports = [
    ./configuration.nix
    ./hardware-configuration.nix
  ];

  home-manager = {
    useGlobalPkgs = true;
    useUserPackages = true;

    users.nick = {
      imports = [
        ../../modules/git.nix
        ../../modules/shell.nix
        ../../modules/vim.nix
      ];
    };
  };
}
