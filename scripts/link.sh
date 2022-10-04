#!/bin/sh
set -e

. ./bin/bootstrap

if [ "$1" != "add" ] && [ "$1" != "del" ]; then
  echo "Must specify 'add' or 'del'"
  exit 1
fi

if [ -z "$2" ]; then
  echo "Must provide dotfiles package name"
  exit 1
fi

if [ ! -e "$DOTFILES_DIR/$2" ]; then
  echo "'$2' is not a valid dotfiles package name"
  exit 1
fi

TARGET="$DOTFILES_DIR/$2"
DEST="$XDG_CONFIG_HOME/$2"

EXISTS=false
if [ -e "$DEST" ]; then
  EXISTS=true
fi

if [ "$1" = "add" ]; then
  if $EXISTS; then
    echo "$TARGET is already linked to $DEST"
  else
    echo "Linking $TARGET to $DEST"
    ln -s "$TARGET" "$XDG_CONFIG_HOME"
  fi
else
  if $EXISTS; then
    echo "Unlinking $DEST"
    rm "$DEST"
  else
    echo "$TARGET is not linked"
  fi
fi
