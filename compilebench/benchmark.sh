#!/bin/sh

help() {
	cat << EOF
This script benchmarks CMLC, MosML, MLton in their compile times.

These results are stored in files (one for each compiler).

Usage:
	$0 <cmlc location>
EOF
}

if [ "$@" -ne 1 ]; then
	help
	exit 1
fi

runs=100

# Benchmark CMLC.  100 Runs of each file.
./compilebench.py --executable="$($1)" --cmlc --runs=$runs --output=cmlc_output.json

# Benchmark MLton.
./compilebench.py --mlton --runs=$runs --output=mlton_output.json

# Benchmark MosML.
./compilebench.py --mosml --runs=$runs --output=mosml_output.json
