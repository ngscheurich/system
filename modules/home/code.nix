# ----------------------------------------------------------
# Packages for advanced source code editing
# ----------------------------------------------------------

{ pkgs, ... }:

{
  home.packages = with pkgs; [
    # Core
    neovim
    ctags
    tree-sitter

    # LSP servers
    lexical
    nil

    # Formatting tools
    black
    fnlfmt
    nixfmt-rfc-style
    nodePackages_latest.eslint_d
    nodePackages_latest.prettier
    nodePackages_latest.stylelint
    pgformatter
    pyright
    sleek
    sqlfluff
  ];
}
