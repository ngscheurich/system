#!/usr/bin/env bash

set -euo pipefail

if [ -z "$SYSTEM_DIR" ]; then
	SYSTEM_DIR=/etc/system
fi

read -p "Install system to $SYSTEM_DIR? (Y/n) " -n 1 -r
echo

if [[ $REPLY =~ ^[Yy]$ ]]; then
	readonly SYSTEM_REPO=git@github.com:ngscheurich/system.git

	# Clone repository
	git clone $SYSTEM_REPO/tmp/system
	sudo mv /tmp/system "$SYSTEM_DIR"

	# Change to system directory
	cd "$SYSTEM_DIR"

	# Initialize and update Git submodules
	git submodule init
	git submodule update

	# Finish
	printf "\033[1m\033[32m Done!\033[0m Installed system files to \033[4m%s\033[0m.\n" \
		"$SYSTEM_DIR"
	cd - >/dev/null
fi
