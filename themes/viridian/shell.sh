#!/usr/bin/env bash

set -euo pipefail

FZF_COLOR="fg:7,bg#264648,hl:8,fg+:3,bg+#264648,hl+:1,gutter#264648,info:6,prompt:2,pointer:4,marker:1,spinner:5"
export FZF_DEFAULT_OPTS="--color $FZF_COLOR"
