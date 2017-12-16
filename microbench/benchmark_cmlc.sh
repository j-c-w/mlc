#!/bin/bash

set -e

usage() {
	cat <<EOF
	Usage:

	$0 <Machine Name> <rev number> <Compiler (Absolute path)>
EOF
}

if [ "$#" -ne 3 ]; then
	usage
	exit  1
fi

version=$2
compiler=$3

run_microrun() {
	local machine_name="$1"
	local json_dump="$2"
	local additional_opts="$3"

	./microrun.py --cmlc --machine-name "$machine_name" --project-version=$version --runs 5 --run-use-perf ./reports/current --json-dump ./reports/current/$json_dump --compiler "$compiler" $additional_opts
}

source lnt-install/bin/activate

# Clear the reports directory:
rm -f ./reports/current/*.{perf,json}

# This program benchmarks CMLC.
run_microrun "$1-cmlc" "cmlc-no-jit.json" ""

# Then run with JIT
run_microrun "$1-cmlc-jit" "cmlc-with-jit.json" "--use-jit"

# And now do both, again, but with optimizations enabled
run_microrun "$1-cmlc-optimized-no-jit" "cmlc-optimize.json" "--compile-options o"

run_microrun "$1-cmlc-optimized-with-jit" "cmlc-optimize-with-jit.json" "--compile-options o --use-jit"

deactivate
