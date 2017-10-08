#!/usr/bin/python

import argparse
import atexit
import datetime
import microrun
import os
import re
import subprocess
import sys

# This script takes a benchmark name, and
# executes the benchmark suite on it for all
# the existing benchmarks at all the right commits.


def dates():
    """ Yields a list of dates. Asssumes that 7 Oct 2017 is the start date
        and the current date is the end date.  """

    date = datetime.datetime(year=2017, day=7, month=10)
    now = datetime.datetime.now()

    while date <= now:
        yield date

        date += datetime.timedelta(days=1)


def log_until(date):
    return subprocess.check_output(['git', 'log', '--pretty="%h"',
                                    '--until', '"' + date.ctime() + '"'])


def hash_for(date):
    output = log_until(date)

    if len(output) == 0:
        print "Error: output of git log --until", date.ctime(), " is empty"
        sys.exit(1)

    return output.split('\n')[0].split('"')[1::2][0]


def revision_number_for(hash):
    return subprocess.check_output(['git', 'rev-list', '--count', str(hash)])


def checkout(hash):
    subprocess.call(['git', 'checkout', hash])


def build():
    subprocess.call(['sbt', 'one-jar'])


def clean_build():
    subprocess.call(['sbt', 'clean'])


def cleanup():
    # Checkout the most recent commit again.
    subprocess.call(['git', 'checkout', 'master'])


if __name__ == "__main__":
    atexit.register(cleanup)

    parser = argparse.ArgumentParser(description="""This script takes
    a benchmark and runs it on all the versions (last commit before
    midnight) that the other benchmarks have been run on. The results
    are output into an LNT compatiable JSON file. """)

    parser.add_argument('--benchmark', required=True,
                        dest='benchmark', action='store',
                        help="The benchmark to run. ")
    parser.add_argument('--machine-name', dest='machine_name',
                        action='store', required=True,
                        help="""The machine name to use when creating records
                        of performance.""")
    parser.add_argument('--compiler', dest='compiler',
                        action='store', required=True,
                        help="The executable to use")
    parser.add_argument('--compile-options', dest='compile_options',
                        default=None,
                        action='store', help="""Options to benhmark with""")
    parser.add_argument('--runtime-options', dest='runtime_options',
                        default=None,
                        action='store', help="""Options to benhmark with""")
    parser.add_argument('--number', dest='number', action='store',
                        default=10, type=int, help="""The number of
                        runs to execute.  """)
    parser.add_argument('--run-use-perf', dest='run_perf', action='store_true',
                        default=False, help="""Use Perf to record
                        performance counters.""")
    parser.add_argument('--name-prefix', dest='name_prefix',
                        action='store', default='',
                        help='A prefix to put on the name of each test.')
    parser.add_argument('--use-jit', dest='use_jit',
                        action='store_true', default=False,
                        help="""If running the executable on the JVM, use
                        JIT compilation.""")

    args = parser.parse_args()

    benchmark_folders = microrun.find_benchmarks(args.benchmark)

    compile_options = []
    runtime_options = []

    if args.run_perf:
        print "Use perf isn't yet implemented"
        sys.exit(1)

    if args.compile_options:
        compile_options = args.compile_options.split(' ')

    if args.runtime_options:
        runtime_options = args.runtime_options.split(' ')

    compiler = microrun.CMLC(executable=args.compiler,
                             use_jit=args.use_jit)
    # Clean the build so that a compiler is always built
    # on the first run
    clean_build()

    for date in dates():
        hash = hash_for(date)
        project_version = revision_number_for(hash)
        checkout(hash)
        os.chdir('..')
        build()
        os.chdir('microbench')

        assert compiler is not None

        starttime = datetime.datetime.now().isoformat()

        gathered_data = \
            microrun.execute_benchmarks(compiler, benchmark_folders,
                                        compile_options, runtime_options,
                                        args.run_perf, args.number,
                                        args.machine_name, project_version,
                                        args.name_prefix)

        print json.dumps(gathered_data.to_dictionary(starttime=starttime),
                         sort_keys=True, indent=4)
        with open('reports/current/catchup-' + date.ctime(), 'w') as f:
            json.dump(gathered_data.to_dictionary(), f)
