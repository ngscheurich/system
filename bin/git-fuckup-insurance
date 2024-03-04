#!/usr/bin/env bash

set -euo pipefail

current_branch=$(git rev-parse --abbrev-ref HEAD)
current_time=$(date +%s%3N)
branch_name="fuckup-insurance-$current_time--$current_branch"

git co -b "$branch_name"
git co -
