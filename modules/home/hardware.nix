# ----------------------------------------------------------
# Packages for working with hardware devices.
# ----------------------------------------------------------

{ pkgs, ... }:

{
  home.packages = with pkgs; [
    tio
  ];
}
