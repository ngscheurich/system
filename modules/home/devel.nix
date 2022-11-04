# ----------------------------------------------------------
# Packages for building and running software
# ----------------------------------------------------------

{ pkgs, ... }:

{
  home.packages = with pkgs; [
    # Languages 
    asdf-vm
    fennel
    beam.interpreters.erlangR25
    beam.packages.erlangR25.elixir_1_14

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
