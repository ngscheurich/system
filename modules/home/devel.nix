# ----------------------------------------------------------
# Packages for building and running software
# ----------------------------------------------------------

{ pkgs, ... }:

{
  home.packages = with pkgs; [
    # Languages 
    elixir
    erlang_27
    fennel
    go
    lua
    nodejs
    # python39
    rustc
    cargo

    # Language support
    black
    fnlfmt
    nil
    pyright
    sleek
    sqlfluff

    # Database tools
    freetds
    pgformatter
    sqlite-interactive

    # Hardware
    tio

    # AI
    aichat
    aider-chat
    tgpt

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

    # Image manipulation
    imagemagick
  ];
}
