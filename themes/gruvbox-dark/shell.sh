#!/usr/bin/env bash

set -euo pipefail

readonly FZF_COLOR="fg:7,bg:#1d2021,hl:8,fg+:3,bg+:#1d2021,hl+:1,gutter:#1d2021,info:6,prompt:2,pointer:4,marker:1,spinner:5"
export FZF_DEFAULT_OPTS="--color $FZF_COLOR"
