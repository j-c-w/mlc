#!/bin/bash

set -e
set -u

# Do this just for one benchmark at a time.
benchmark_pat="mandelbrot"
opts="--f-byte-dce,dce --f-copy-prop,copyprop --f-peephole,peephole --f-simplify,simplify --f-t-inline,inline --f-tce,tce"

usage() {
	cat <<EOF
	Usage: $0 <cmlc executable>

	This script benchmarks the interaction between passes.

	It tries every combination of passes in $opts (may need to be
	updated) and records benchmarking times.

	Since this is a time consuming process, this is currenlty only done
	for benchmarks matching $benchmark_pat.
EOF
}

if [ "$#" -ne 1 ]; then
	usage
	exit 1
fi

run_with_opts() {
	local compiler="$1"
	local machine_name="$2"
	local compile_opts="$3"

	# We use the JIT to keep run times reasonable.  It is by no means a
	# requirement.
	./microrun.py --cmlc --machine-name "$machine_name" --project-version=1 --runs 10 --run-use-perf ./reports/current --use-jit --benchmarks=$benchmark_pat --json-dump "./reports/current/cmlc-$machine_name.json" --compiler "$compiler" --compile-options="$compile_opts"
}

# Run with all combinations of optimizations.  Store these differently.
compiler="$1"

# Iterate over all squares.
for i in $opts; do
	IFS=","
	read option name <<< ${i}
	# IFS must be set back to default so that the list
	# can be iterated internally.
	IFS=" "
	for j in $opts; do
		IFS=","
		read option_2 name_2 <<< ${j}
		IFS=" "

		if [[ "$name" != "peephole" && "$name_2" != "peephole" ]]; then
			# We have to do this because peepholes are enabled by defualt.
			options="--fno-peephole"
		else
			options=""
		fi

		if [ "$name_2" != "$name" ]; then
			options="$option $option_2 $options"
			combined_name="${name}_$name_2"
	    else
			# The names are identical.  Do not duplicate the arguments.
			options="$option $options"
			combined_name="${name}_$name"
		fi

		# Now, run the compiler:
		echo "Running compiler name $combined_name with opts $options"
		run_with_opts "$compiler" "$combined_name" "$options"
	done
done

# Finally, copy everthing from that run into a special folder.
mkdir -p ./reports/pairwise_comparison
mv ./reports/current/*.{json,perf} ./reports/current/pairwise_comparison
