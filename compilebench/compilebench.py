#!/usr/bin/python

import argparse
import json
import os
import re
import subprocess
import time


class Compiler(object):
    def __init__(self):
        pass

    def compile(self, filename, options, times):
        """ This is expected to fail if the compilation fails.  """
        raise Exception("Expected compile to be implemented.")


class MosML(Compiler):
    def __init__(self):
        pass

    def compile(self, filename, options, times):
        print "Compiling", filename, "with MLton"
        deduplicated_options = options
        deduplicated_options = [x for x in deduplicated_options if x]
        commands = ['time', 'mosmlc', filename, '-o', 'time_data'] + \
            deduplicated_options
        start_millis = int(round(time.time() * 1000))
        output = subprocess.check_output(commands)
        end_millis = int(round(time.time() * 1000))

        if 'subprocess_times' not in times:
            times['subprocess_times'] = []

        times['subprocess_times'].append(end_millis - start_millis)
        return times


class MLton(Compiler):
    def __init__(self):
        pass

    def compile(self, filename, options, times):
        print "Compiling", filename, "with MLton"
        deduplicated_options = options
        deduplicated_options = [x for x in deduplicated_options if x]
        commands = ['time', 'mlton', filename] + deduplicated_options
        output = subprocess.check_output(commands)

        start_millis = int(round(time.time() * 1000))
        output = subprocess.check_output(commands)
        end_millis = int(round(time.time() * 1000))

        if 'subprocess_times' not in times:
            times['subprocess_times'] = []

        times['subprocess_times'].append(end_millis - start_millis)
        return times


class CMLC(Compiler):
    def __init__(self, executable):
        self.executable = executable

    def compile(self, filename, options, times):
        """ This takes a filename to compile and options to compile it with.
            'times' is a results dictionary that is used to store the times.
            If times have already been entered on a previous run, then they
            can be added to.  """
        print "Compiling", filename
        deduplicated_options = options + ['--compile-stats']
        # TODO -- actually deduplicate these options.
        deduplicated_options = [x for x in deduplicated_options if x]
        commands = [self.executable] + deduplicated_options + [filename]

        start_millis = int(round(time.time() * 1000))
        output = subprocess.check_output(commands)
        end_millis = int(round(time.time() * 1000))

        if 'subprocess_times' not in times:
            times['subprocess_times'] = []

        times['subprocess_times'].append(end_millis - start_millis)

        for line in output.split('\n'):
            # 'line' is of the form:
            #       "Time for pass 'pass_name' = XXXms"
            # and the last line is of the form:
            #       "Compile time: XXXms
            if line.startswith("Time for pass '"):
                line = line[len("Time for pass '"):]

                # Extract the pass name:
                passname = line[:line.find("'")]
                # And the time:
                this_time = line[line.find("=") + 1:].strip()[:-2]

                time_number = int(this_time)

                if passname in times:
                    times[passname].append(time_number)
                else:
                    times[passname] = [time_number]
            elif line.startswith("Compile time: "):
                this_time = int(line[len("Compile time: "):-2])

                if 'total' in times:
                    times['total'].append(this_time)
                else:
                    times['total'] = [this_time]
                print "Total time ", this_time

        return times


def sort_files(files):
    # Taken from: stackoverflow question 5967500
    def atoi(text):
        return int(text) if text.isdigit() else text

    def natural_keys(text):
        return [atoi(c) for c in re.split('(\d+)', text)]

    files.sort(key=natural_keys)


def docstring():
    return """This is a script for running compile time benchmarks."""


def execute_test(name, options, runs, compiler):
    print("Executing test for " + name)
    # Change into the directory for the name.
    os.chdir('benchmarks/' + name)

    # Get the executable script:
    gen_script = './gen'
    # Execute the generating script:
    subprocess.call([gen_script], shell=True)

    # Now get all the build files.  Build each 'runs' times consecutively.
    files = [file for file in os.listdir('.') if re.match('[0-9]+\.sml', file)]
    sort_files(files)
    print "got files ", files

    results = {}
    results['number'] = len(files)
    results['runs'] = runs
    results['name'] = ''

    if "-O" in options or "--optimize" in options:
        results['name'] = 'optimize'

    for run in range(runs):
        for file in files:
            if file in results:
                already_gathered = results[file]
            else:
                already_gathered = {}
            results[file] = compiler.compile(file, options, already_gathered)

    # Go back out
    os.chdir('../..')
    print "Finished tests for " + name

    return results


if __name__ == "__main__":
    parser = argparse.ArgumentParser(description="Run compile time benchmarks")

    parser.add_argument('--filter', dest='benchmark_filter', action='store',
                        default=None, help=('Only run benchmarks whose name '
                                            'matches the regex passed.'))
    parser.add_argument('--runs', dest='runs', action='store',
                        type=int,
                        default=5, help=('Number of times to build each'
                                         ' benchmark.'))
    parser.add_argument('--options', dest='options', action='store',
                        default='', help=('Options to set on every'
                                          ' compilation instance. '))
    parser.add_argument('--output', dest='output_file', action='store',
                        default='benchmark_data.json',
                        help='Output file to use.')
    parser.add_argument('--executable', dest='executable', action='store',
                        default=None, help='Compiler executable to use')
    compilers_group = parser.add_mutually_exclusive_group(required=True)
    compilers_group.add_argument('--cmlc', help='Run CMLC', dest='use_cmlc',
                                 default=False, action='store_true')
    compilers_group.add_argument('--mosml', help='Run MosML', dest='use_mosml',
                                 default=False, action='store_true')
    compilers_group.add_argument('--mlton', help='Run MLton', dest='use_mlton',
                                 default=False, action='store_true')
    args = parser.parse_args()

    compiler = None
    if args.use_cmlc:
        if not args.executable:
            raise Exception("Need to specify the compiler executable for cmlc")
        compiler = CMLC(args.executable)
    elif args.use_mosml:
        compiler = MosML()
    elif args.use_mlton:
        compiler = MLton()
    else:
        raise Exception("Unknown compiler used.")

    tests = [dir for dir in os.listdir('./benchmarks')
             if os.path.isdir('benchmarks/' + dir)]

    if args.benchmark_filter:
        regex = re.compile(args.benchmark_filter)
        tests = [test for test in tests if regex.match(test)]

    options = args.options.split(' ')
    output_dicts = {}

    for test in tests:
        output_dicts[test] = execute_test(test, options, args.runs, compiler)

    with open(args.output_file, 'w') as f:
        json.dump(output_dicts, f)
