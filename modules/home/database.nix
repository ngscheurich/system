# ----------------------------------------------------------
# Packages for working with databases
# ----------------------------------------------------------

{ pkgs, ... }:

{
  home.packages = with pkgs; [
    pgcli
  ];
}
