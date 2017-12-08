#!/bin/bash

# This is a script that runs the testsuite for
# cmlc. It builds the compiler using sbt into a jar file,
# then executes the test suite using the python testbench
# file and some default settings.

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

echo $(pwd)
cd ..
echo $(pwd)
# Clean is run so that the one-jar is actually rebuilt.
# Without clean being run, an unchanged project from the 
# last one-jar build would result in no path to the 
# jarfile being printed.
sbt clean
# Note that -f starts counting from 1
sbt_output=$(sbt one-jar)

if [ $(grep "^[error]" <<< "$sbt_output" | wc -l) -ne 0 ]; then
	# There was a compile error
	echo "Compiler error"
	exit 255
fi

jarfile=$(grep "Packaging" <<< "$sbt_output" | grep "one-jar" | cut -d' ' -f 3)

cd test

# Execute the testsuite
./run_test.py --executable="java -jar $jarfile"

# Check for all passes:
no_failures=$(grep -o 'FAIL' test.res | wc -l)

exit $no_failures
