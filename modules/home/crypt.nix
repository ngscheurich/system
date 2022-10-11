# ----------------------------------------------------------
# Packages for encrypting and signing data
# ----------------------------------------------------------

{ pkgs, ... }:

{
  home.packages = with pkgs; [
    gnupg
    gpg-tui
    libressl
  ];
}
