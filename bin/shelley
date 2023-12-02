#!/usr/bin/env bash

set -euo pipefail

readonly VERSION="0.1.0"
readonly DATA_DIR="$XDG_DATA_HOME/shelley"
readonly DATA_FILE="$DATA_DIR/shelley.db3"

__execute() {
	sqlite3 "$DATA_FILE" "$1"
}

__help_banner() {
	cat <<EOF
Shelley; or, Helpers for Nix Shells
Version $VERSION
EOF
}

## =========================================================
## Subcommand (db)
## =========================================================

db__help() {
	cat <<EOF
$(__help_banner)

USAGE:
    shelley db [OPTION]
    shelley db [COMMAND]

OPTIONS:
    -h, --help    Print help information

COMMANDS:
    enter     Register a server for the \$PWD
    init      Initialize the database file
    leave     Remove a server registration for the \$PWD
    status    View server registrations
EOF
}

db__init() {
	if [ -f "$DATA_FILE" ]; then
		echo "Database file already exists"
	else
		mkdir -p "$DATA_DIR"
		touch "$DATA_FILE"
		__execute "CREATE TABLE servers(path text)"
		echo "Database file created"
	fi
}

db__status() {
	if [ -f "$DATA_FILE" ]; then
		__execute "SELECT * FROM servers"
	else
		echo "Database file does not exist"
	fi
}

db__enter() {
	mkdir -p "$PGHOST"

	if [ ! -d "$PGDATA" ]; then
		printf "[postgresql] initializing database.... "
		initdb --auth=trust >/dev/null
		printf "done\nserver initialized\n"
	fi

	if ! pg_ctl status >/dev/null; then
		printf "[postgresql] "
		pg_ctl -o "--unix_socket_directories='$PGHOST'" -l postgresql.log start
	fi

	if ! psql -c '\du' | grep '^ postgres' >/dev/null; then
		printf "[postgresql] creating postgres superuser.... "
		createuser -s postgres
		printf "done\nuser created\n"
	fi

	__execute "INSERT INTO servers VALUES('$PWD')"
}

db__leave() {
	__execute "DELETE FROM servers WHERE path = '$PWD' LIMIT 1"

	if [ "$(__execute 'SELECT * FROM servers' | wc -l)" = '0' ]; then
		printf "[postgresql] "
		pg_ctl stop
	fi
}

subcommand_db() {
	case "$1" in
	"-h" | "--help")
		db__help
		;;
	"init")
		db__init
		;;
	"status")
		db__status
		;;
	"enter")
		db__enter
		;;
	"leave")
		db__leave
		;;
	*)
		db__help
		;;
	esac
}

## =========================================================
## Main
## =========================================================

main__help() {
	cat <<EOF
$(__help_banner)

USAGE:
    shelley [OPTION]
    shelley [COMMAND]

OPTIONS:
    -h, --help       Print this information
    -v, --version    Print version information

COMMANDS:
    db    PostgreSQL helpers
EOF
}

main__version() {
	echo "shelley $VERSION"
}

case "$1" in
"-h" | "--help")
	main__help
	;;
"-v" | "--version")
	main__version
	;;
"db")
	subcommand_db "$2"
	;;
*)
	main__help
	;;
esac
