#!/usr/bin/env bash

set -euo pipefail

FZF_COLOR="fg:7,bg:#131a24,hl:11,fg+:3,bg+:#131a24,hl+:1,gutter:#131a24,info:6,prompt:2,pointer:4,marker:1,spinner:5"
export FZF_DEFAULT_OPTS="--color $FZF_COLOR"
