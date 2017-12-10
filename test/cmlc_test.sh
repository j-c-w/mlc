#!/bin/bash

# This is a script that runs the testsuite for
# cmlc. It builds the compiler using sbt into a jar file,
# then executes the test suite using the python testbench
# file and some default settings.

set -e
set -u

usage() {
	cat <<EOF
This is a script for running the testsuite. It executes each of the
tests in the directory.

This script takes no options. (except -h to show this)
EOF
}

# Parse the arguments
while [[ $# -gt 0 ]]
do
	key = "$1"

	case $key in
		-h | --help)
			usage
			exit 0
			;;
	esac
done

# Arguments handled. We assume that we are running from
# within the test directory.

installation_location="$(pwd)/cmlc_installation"
executable="$(pwd)/cmlc"

echo $(pwd)
cd ..
echo $(pwd)

# Run the install script:
zsh ./install.sh "$installation_location" "$executable"

cd test

# Execute the testsuite
./run_test.py --executable="$executable"

# Check for all passes:
no_failures=$(grep -o 'FAIL' test.res | wc -l)

exit $no_failures
