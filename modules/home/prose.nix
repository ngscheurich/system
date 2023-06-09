# ----------------------------------------------------------
# Packages for composing and viewing natural language files
# ----------------------------------------------------------

{ pkgs, ... }:

{
  home.packages = with pkgs; [
    glow
    md-tangle
    vale
    nodePackages_latest.mermaid-cli
  ];
}
