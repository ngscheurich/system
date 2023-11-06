{ ... }:

{
  users = {
    knownUsers = [ "nscheurich" ];

    users.nick = {
      home = "/Users/nscheurich";
      description = "N. G. Scheurich";
      shell = pkgs.fish;
      uid = 501;
    };
  };

  home-manager.users = {
    nscheurich = {
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
}
