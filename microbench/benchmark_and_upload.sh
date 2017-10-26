#!/bin/bash

# This program benchmarks CMLC and uploads that
# to LNT.
# We assume that the lnt_install.sh script has already
# been run.

if [ "$#" -ne 1 ]; then
	echo <<EOF
	Usage: $0 <Machine Name>
EOF
fi

rev_number=$(git rev-list --count HEAD)

./benchmark_cmlc.sh $1 $rev_number
./benchmark_mosml.sh $1 $rev_number

./lnt_import.sh
