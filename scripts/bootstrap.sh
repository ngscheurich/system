#!/bin/sh
set -e

if [ -z "$SYSTEM_DIR" ]; then
  export SYSTEM_DIR=/etc/system
fi

if [ -z "$XDG_CONFIG_HOME" ]; then
  export XDG_CONFIG_HOME="$HOME/.config"
fi

export BIN_DIR=$SYSTEM_DIR/bin
export DOTFILES_DIR=$SYSTEM_DIR/dotfiles
export SYSTEM_REPO=git@github.com:ngscheurich/system.git
