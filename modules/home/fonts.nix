# ----------------------------------------------------------
# Some nice open-source fonts
# ----------------------------------------------------------

{ pkgs, ... }:

{
  home.packages = with pkgs; [
    julia-mono
    iosevka
    iosevka-commfy
  ];
}
