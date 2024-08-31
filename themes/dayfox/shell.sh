#!/usr/bin/env bash

set -euo pipefail

FZF_COLOR="fg:0,bg:#f2e9e1,hl:11,fg+:3,bg+:#f2e9e1,hl+:1,gutter:#f2e9e1,info:6,prompt:2,pointer:4,marker:1,spinner:5"
export FZF_DEFAULT_OPTS="--color $FZF_COLOR"
