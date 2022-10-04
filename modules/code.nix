# ----------------------------------------------------------
# Packages for advanced source code editing
# ----------------------------------------------------------

{ pkgs, ... }:

{
  home.packages = with pkgs; [
    # Core
    neovim-nightly
    tree-sitter

    # Language servers
    elixir_ls
    nodePackages.bash-language-server
    nodePackages.typescript
    nodePackages.typescript-language-server
    nodePackages.vim-language-server
    nodePackages.vscode-css-languageserver-bin
    nodePackages.vscode-langservers-extracted
    rnix-lsp
    sumneko-lua-language-server
    terraform-ls

    # Formatting/linting
    astyle
    nodePackages.eslint
    nodePackages.eslint_d
    nodePackages.prettier

    # Tools
    ctags
    shellcheck
  ];
}
