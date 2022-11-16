# ----------------------------------------------------------
# Packages for advanced source code editing
# ----------------------------------------------------------

{ pkgs, ... }:

{
  home.packages = with pkgs; [
    # Core
    neovim-nightly
    tree-sitter

    # Tools
    ctags
    nodePackages_latest.eslint_d
    nodePackages_latest.prettier
    nodePackages_latest.stylelint
  ];
}
