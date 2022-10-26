#!/bin/bash

set -e

HOSTNAME=$(hostname)

if [[ $HOSTNAME =~ ^NSCH.*\.local ]]; then
  echo 'ridcully'
else
  echo "$HOSTNAME"
fi
