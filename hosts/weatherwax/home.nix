{ ... }:

{
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
        ../../modules/home/vim.nix
      ];
    };
  };

  homebrew = {
    casks = [
      "affinity-designer"
      "affinity-photo"
      "affinity-publisher"
      "godot"
      "playdate-simulator"
      "unity-hub"
    ];
  };
}
