# ----------------------------------------------------------
# Packages for building and running software
# ----------------------------------------------------------

{ pkgs, ... }:

{
  home.packages = with pkgs; [
    # Languages and runtimes
    deno
    elixir
    erlang_27
    fennel
    go
    lua
    nodejs
    python3Full
    rustc
    cargo

    # Language tools
    lexical
    lua-language-server

    # Database tools
    freetds
    sqlite-interactive

    # Hardware tools
    tio

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
  ];
}
