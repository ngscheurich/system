#!/usr/bin/env bash

set -euo pipefail

HOSTNAME=$(hostname)

if [[ $HOSTNAME =~ ^NSCH.*\.local || $HOSTNAME == "MacBookPro.attlocal.net" ]]; then
	echo "wattson"
else
	echo "$HOSTNAME"
fi
