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
  ];
}
