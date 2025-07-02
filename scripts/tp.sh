#!/bin/sh

DIR="$HOME/.config/tmuxp"

if [ ! -d "$DIR" ]; then
  SIGN=$(gum style --foreground 142 ' ')
  MSG="Directory $DIR does not exist"
  gum join "$SIGN" "$MSG"  >&2
  exit 1
fi

CHOICE=$(fd . --type f $DIR | sed "s|^$DIR/\(.*\).yaml|\1|" | gum filter --limit 1)

tmuxp load "$DIR/$CHOICE.yaml"
