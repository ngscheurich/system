#!/usr/bin/env bash

set -euo pipefail

FZF_COLOR="fg:7,bg:#15161e,hl:11,fg+:3,bg+:#15161e,hl+:1,gutter:#15161e,info:6,prompt:2,pointer:4,marker:1,spinner:5"
export FZF_DEFAULT_OPTS="--color $FZF_COLOR"
