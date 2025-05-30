#!/usr/bin/env bash
# ==========================================================
# Projections Generator for projectionist.vim
# ----------------------------------------------------------
# dbutil is a command-line utility for working with local
# PostgreSQL database systems via UNIX sockets.
#
# The following environment variables must be present for
# this program to function correctly:
#
#   * PGHOST
#   * PGDATA
#   * PGPORT
#   * PGDATABASE
#   * LOG_PATH
#
# ----------------------------------------------------------

set -euo pipefail

readonly VERSION="0.1.0"

ARGS=()
APP_NAME="$(basename $PWD)"

while [[ $# -gt 0 ]]; do
	case "$1" in
  "-a" | "--appname")
    APP_NAME="$2"
    shift
    shift
    ;;
  *)
    ARGS+=("$1")
    shift
    ;;
  esac
done

set -- "${ARGS[@]}"

project_phoenix() {
  PROJECTIONS=$(cat <<EOF
{
  "lib/${APP_NAME}_web/live/*.ex": {
    "alternate": "lib/${APP_NAME}_web/live/{}.html.heex",
    "type": "source",
    "template": [
      "defmodule {camelcase|capitalize|dot}Live do",
      "  use Phoenix.LiveView",
      "",
      "  @impl Phoenix.LiveView",
      "  def mount(_params, _session, socket) do",
      "    \{:ok, socket\}",
      "  end",
      "end"
    ]
  },
  "lib/${APP_NAME}_web/live/*.html.heex": {
    "alternate": "lib/${APP_NAME}_web/live/{}.ex",
    "type": "template",
  },
  "lib/*.ex": {
    "alternate": "test/{}_test.exs",
    "type": "source",
    "template": [
      "defmodule {camelcase|capitalize|dot} do",
      "end"
    ]
  },
  "test/*_test.exs": {
    "alternate": "lib/{}.ex",
    "type": "test",
    "template": [
      "defmodule {camelcase|capitalize|dot}Test do",
      "  use ExUnit.Case, async: true",
      "",
      "  alias {camelcase|capitalize|dot}",
      "end"
    ]
  }
}
EOF
)

  echo "$PROJECTIONS" > .projections.json
}

show_help() {
	cat <<EOF
Projections Generator for projectionist.vim
Version $VERSION

USAGE:
project [COMMAND] [OPTION]

COMMANDS:
phoenix    Create Phoenix projections

OPTIONS:
-a, --appname    Specify app name (defaults to current directory)
-h, --help       Print this information
-v, --version    Print version information
EOF
}

version() {
	echo "project $VERSION"
}

if [[ $# -eq 0 ]]; then
	show_help
else
	case "$1" in
	"-h" | "--help" | "help")
		show_help
		;;
	"-v" | "--version")
		version
		;;
	"phoenix")
		project_phoenix
		;;
	*)
		show_help
		;;
	esac
fi
