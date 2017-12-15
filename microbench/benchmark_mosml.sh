
#!/bin/bash

usage() {
	cat <<EOF
	Usage:

	$0 <Machine Name> <rev number>
EOF
}

if [ "$#" -ne 2 ]; then
	usage
	exit  1
fi

# For this, we treat the version as the UNIX timestamp.
version=$2

# This program benchmarks MOSML.
./microrun.py --mosml --machine-name "$1-mosml" --use-perf ./reports/current --project-version $version --runs 20 --json-dump ./reports/current/mosml_no_perf.json
