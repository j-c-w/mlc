#!/bin/zsh

set -e
set -u

opts=(--f-byte-dce,dce --f-copy-prop,copyprop --f-peephole,peephole --f-simplify,simplify --f-t-inline,inline --f-tce,tce)

usage() {
	cat <<EOF
	This benchamrks the contributions of each of the individual passes.

	Each pass is run without the others for each benchmark.

	Usage: $0 <cmlc executable>
EOF
}

if [ "$#" -ne 1 ]; then
	usage
	exit 1
fi

compiler="$1"

run_with_opts() {
	local compiler="$1"
	local machine_name="$2"
	local compile_opts="$3"

	# Use with JIT and without JIT requirement.
	./microrun.py --cmlc --machine-name "$machine_name" --project-version=1 --runs 20 --run-use-perf ./reports/current --use-jit --json-dump "./reports/current/cmlc-$machine_name.json" --compiler "$compiler" --compile-options="$compile_opts"
	./microrun.py --cmlc --machine-name "$machine_name" --project-version=1 --runs 6 --run-use-perf ./reports/current --json-dump "./reports/current/cmlc-$machine_name-no-jit.json" --compiler "$compiler" --compile-options="$compile_opts"
}

for i in $opts; do
	IFS=","
	read opt name <<< ${i}
	IFS=" "

	if [ $name != "peephole" ]; then
		compile_options="--fno-peephole $opt"
	else
		compile_options="$opt"
	fi

	echo "Runninng compiler $name with options $compile_options"

	run_with_opts $compiler $name $compile_options
done

mkdir -p reports/individual_passes
mv ./reports/current/* reports/individual_passes
