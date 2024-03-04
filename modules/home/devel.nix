# ----------------------------------------------------------
# Packages for building and running software
# ----------------------------------------------------------

{ pkgs, ... }:

{
  home.packages = with pkgs; [
    # Languages 
    asdf-vm
    beam.packages.erlangR25.elixir_1_15
    erlangR25
    fennel
    go
    lua
    nodejs
    python39
    rustc
    cargo

    # Language support
    rnix-lsp

    # Database tools
    freetds
    pgformatter
    sqlite-interactive

    # Hardware
    tio

    # Networking
    httpie
    ngrok
    websocat

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
    pkg-config

    # Libraries
    glib
    gum
  ];
}
