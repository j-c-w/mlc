#!/bin/bash

set -e

# This program benchmarks CMLC and uploads that
# to LNT.
# We assume that the lnt_install.sh script has already
# been run.

if [ "$#" -ne 2 ]; then
	echo <<EOF
	Usage: $0 <Machine Name> <CMLC executable path>
EOF
fi

# Make sure that the directory structure is in place:
mkdir -p reports/current

lockfile=.benchmark_lockfile

if ( set -o noclobber; echo "$$" > "$lockfile") 2> /dev/null; then
	trap 'rm -f "$lockfile"; exit $?' INT TERM EXIT

	rev_number=$(git rev-list --count HEAD)

	./benchmark_cmlc.sh "cmlc-$1" $rev_number "$2"
	./lnt_import.sh

	# Sleep long enough to ensure a different timestamp on the directory.
	sleep 60
	./benchmark_mosml.sh "mosml-$1" $rev_number
	./lnt_import.sh

	# clean up after yourself, and release your trap
	rm -f "$lockfile"
	trap - INT TERM EXIT
else
	echo "Lock Exists: $lockfile owned by $(cat $lockfile)"
fi
