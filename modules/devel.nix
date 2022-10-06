# ----------------------------------------------------------
# Packages for building and running software
# ----------------------------------------------------------

{ pkgs, ... }:

{
  home.packages = with pkgs; [
    # Languages 
    fennel

    # Database tools
    freetds

    # Containerization/virtualization
    docker-machine

    # Build tools
    autoconf
    automake
    cmake
    gettext
    gradle
    libressl
    libtool
    mkcert
    ninja
    nss
    pkgconfig

    # Libraries
    glib
  ];
}
