# ----------------------------------------------------------
# Packages for building and running software
# ----------------------------------------------------------

{ pkgs, ... }:

{
  home.packages = with pkgs; [
    # Languages 
    asdf-vm
    beam.packages.erlangR25.elixir_1_14
    erlangR25
    fennel
    nodejs

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
