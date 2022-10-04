#!/bin/sh
set -e

if [ "$(uname)" = "Darwin" ]; then
  HOST_OS="darwin"
  SWITCH_CMD="./result/sw/bin/darwin-rebuild switch --flake .#"
else
  HOST_OS="nixos"
  SWITCH_CMD="sudo nixos-rebuild switch --flake .#"
fi

cd /etc/system

nix build ".#${HOST_OS}Configurations.$(hostname).system"

eval "$SWITCH_CMD"

cd -
