{ pkgs, ... }:

{
  imports = [
    ./configuration.nix
    ./preferences.nix
    ./homebrew.nix
  ];

  home-manager = {
    useGlobalPkgs = true;
    useUserPackages = true;

    users.nscheurich = {
      imports = [
        ../../modules/code.nix
        ../../modules/crypt.nix
        ../../modules/devel.nix
        ../../modules/devops.nix
        ../../modules/git.nix
        ../../modules/prose.nix
        ../../modules/shell.nix
        ../../modules/term.nix
        ../../modules/vim.nix
      ];
    };
  };
}
