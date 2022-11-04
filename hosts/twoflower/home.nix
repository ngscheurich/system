{ ... }:

{
  home-manager = {
    users.nick = {
      imports = [
        ../../home/modules/git.nix
        ../../home/modules/shell.nix
        ../../home/modules/vim.nix
      ];
    };
  };
}
