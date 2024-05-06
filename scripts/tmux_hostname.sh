#!/usr/bin/env bash

set -euo pipefail

readonly HOSTNAME

HOSTNAME=$(hostname)

if [[ $HOSTNAME =~ ^NSCH.*\.local ]]; then
	echo "ridcully"
else
	echo "$HOSTNAME"
fi
