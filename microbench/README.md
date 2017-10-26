# Running the testsuite:

This is done with the 'microrun.py' file.

Execute:

	./microrun.py (--smlnj|--mosml|--cmlc)

To run the benchmarks with that compiler (--help) for more options.


# Added a new benchmark:

The trouble with the benchmarks is that they must be written
in a way that all the benchmarked compilers can run. See
existing benchmarks for examples.

Benchmarks should be placed in the 'benchmark/BENCHMARK_NAME/' folder.

However, all benchmarks should define an entry-point (a `main` function),
which is independent of the compiler. A sources.cm file should be 
provided for smlnj to use.

The `main` function should:

1. Record and output the time that the program took to execute.
2. Validate that the output is correct given the program input.

The output (printed to stdout) should be of the form:

	Execution Time: ___ (in fractional seconds)
	Validation: ___ (pass/fail)

# The purpose of each script in this folder:
	
 - benchmark_and_upload.sh <Machine Name>: This executes the
benchmark shell scripts below for each of the individual compilers.
These are then uploaded into an LNT database.

 - benchmark_cmlc <Machine Name>: This execute the benchmarking script for
	CMLC.

 - benchmark_mosml <Machine Name>: This execute the benchmarking script for
	MOSML.

 - lnt_import.sh: This looks in the 'reports/current' folder for any '.json'
	files. These files are imported into LNT. The files are then moved
	to a folder named 'prev-DATE'.

 - lnt_install.sh: This is the first thing that should be run. It downloads
	and installs LNT into the directory.


# Python scripts:
These do the heavyweight processing here.

- microrun.py:
 (./microrun.py --help for options).

	This takes the options provided and runs each of the benchmarks
	using the appropriate compiler/runtime.

	It extracts the runtime results from each. It also measures
	compilation time. These results are output in JSON format
	suitable for LNT import. It is also suitable for further analysis.

	
- compare.py:
	(./compare.py --help for options).

	This takes the compilers provided as input and runs the benchmarks
	on them. It produces a variety of graphs that depict the differences
	in runtimes between the compilers.

 - new_benchmark.py:
	(./new_benchmark.py --help for options).

	This takes a new benchmark specified and runs *all* the
	used git commits (i.e. the last commit before midnight
	for each date) up to a specified start date.

- compare.py:
	(./compare.py --help for options).

	This takes two compilers. It takes a filter of benchmarks.
	It draws graphs comparing the compilers.
