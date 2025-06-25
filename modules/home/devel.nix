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
    python3Full
    rustc
    cargo

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
