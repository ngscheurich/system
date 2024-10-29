# ----------------------------------------------------------
# Packages for building and running software
# ----------------------------------------------------------

{ pkgs, ... }:

{
  home.packages = with pkgs; [
    mu
  ];
}