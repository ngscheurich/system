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
    go
    lua
    nodejs
    python39
    rustc
    cargo

    # Database tools
    freetds
    sqlite-interactive

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
    gum
  ];
}
