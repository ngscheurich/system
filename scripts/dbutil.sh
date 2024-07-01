#!/usr/bin/env bash
# ==========================================================
# Database Utilities
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
readonly DB_NAME="gridpoint_dev"
readonly LABEL="[dbutil]"

_log() {
	echo "$LABEL $1"
}

_log_start() {
	printf "%s %s.... " "$LABEL" "$1"
}

_log_done() {
	printf "done\n%s\n" "$1"
}

init() {
	if [ ! -d "$PGDATA" ]; then
		mkdir -p "$PGDATA"
		_log_start "initializing database"
		initdb --auth=trust >/dev/null
		_log_done "database initialized"
	else
		if [[ $# -eq 0 || "$1" != "-s" ]]; then
			_log "database already initialized"
		fi
	fi
}

bootstrap() {
	if ! psql -c '\du' | grep '^ postgres' >/dev/null; then
		_log_start "creating postgres superuser"
		createuser -s postgres
		_log_done "user created"
	fi
}

start() {
	init -s

	if ! pg_ctl status >/dev/null; then
		printf '%s ' "$LABEL"
		mkdir -p "$LOG_PATH"
		pg_ctl \
			--options "--unix_socket_directories='$PGHOST'" \
			--options "--port='$PGPORT'" \
			--log "$LOG_PATH/postgresql.log" \
			start
	else
		_log "server already started"
	fi

	bootstrap
}

stop() {
	echo -n "$LABEL "
	pg_ctl stop
}

dump() {
	pg_dump -d "$DB_NAME" >"$1" 2>/dev/null
	echo "Data has been dumped to $1"
}

load() {
	read -rp "This will replace your local develoment database. Proceed? (Y/n) " choice
	local choice=${choice:-Y}
	if [ "$choice" = "Y" ] || [ "$choice" = "y" ]; then
		"mix do ecto.drop, ecto.create"
		psql "$DB_NAME" <"$1" 1>/dev/null 2>/dev/null
		echo "Data has been loaded from $1"
	fi
}

version() {
	echo "dbutil $VERSION"
}

show_help() {
	cat <<EOF
Database Utilities
Version $VERSION

USAGE:
dbutil [OPTION]
dbutil [COMMAND]

OPTIONS:
-h, --help       Print this information
-v, --version    Print version information

COMMON COMMANDS:
start          Start local database server
stop           Stop local database server
dump [PATH]    Dump development database to [PATH]
load [PATH]    Load development database from [PATH]

OTHER COMMANDS:
help         Print this message
init         Initialize the database file
bootstrap    Create 'postgres' superuser
EOF
}

if [[ $# -eq 0 ]]; then
	show_help
else
	case "$1" in
	"-v" | "--version")
		version
		;;
	"-h" | "--help" | "help")
		show_help
		;;
	"init")
		init
		;;
	"bootstrap")
		bootstrap
		;;
	"start")
		start
		;;
	"stop")
		stop
		;;
	"dump")
		dump "$@"
		;;
	"load")
		load "$@"
		;;
	*)
		show_help
		;;
	esac
fi
