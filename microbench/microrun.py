#!/usr/bin/python

import argparse
import datetime
import fnmatch
import json
import numpy as np
import os
import subprocess
import sys
import time


class Compiler(object):
    """ The subclasses of compiler are used to encapsulate the
        individual build problems of each possible ML compiler.

        Subclasses are expected to implement:

            compile(filename, options): Given a file of filename to compile,
            compile it with the options provided.

            run(options, use_perf): Execute the file we expect to be
            produced by the compile function. This class (the parent)
            deals with the difficulties of managing 'perf' and taskset.
            Construct the command, and call Compiler.run(...).  """

    def __init__(self):
        pass

    def run(self, command, use_perf, source_filename):
        if use_perf:
            fields = ['branch-instructions', 'branch-misses',
                      'cpu-clock', 'page-faults', 'alignment-faults',
                      'cpu-cycles', 'L1-dcache-load-misses',
                      'L1-dcache-loads', 'L1-dcache-stores',
                      'L1-dcache-store-misses', 'L1-dcache-prefetch-misses',
                      'L1-icache-load-misses', 'dTLB-load-misses',
                      'dTLB-loads', 'dTLB-store-misses', 'branch-loads',
                      'branch-load-misses', 'bus-cycles', 'cache-references',
                      'cache-misses', 'instructions', 'mem-loads',
                      'mem-stores', 'stalled-cycles-backend',
                      'stalled-cycles-frontend']

            command = ['perf', 'stat', '-e', ",".join(fields),
                       '-o', 'data.perf'] + command

        try:
            return subprocess.check_output(['taskset', '-c', '1'] + command)
        except subprocess.CalledProcessError as e:
            print "Run of task failed"
            print e
            return None

    def compile(self, command, input_filename):
        # Delete the old executable if it exists
        if os.path.exists(self.get_generated_executable(input_filename)):
            os.remove(self.get_generated_executable(input_filename))

        print "compiler is " + str(command)
        # We use a rough approximation to the compilation time here.
        start = time.time()
        try:
            subprocess.call(command)
        except subprocess.CalledProcessError as e:
            print "Build failed"
            print e
            # We allow any kind of error to fail silently here.
            # This indicates that the build failed.
            return None
        return time.time() - start


class CMLC(Compiler):
    def __init__(self, executable=['cmlc'], use_jit=False):
        self.executable = executable
        self.jit = use_jit

    def compile(self, filename, options):
        return Compiler.compile(self, self.executable + options + [filename],
                                filename)

    def get_generated_executable(self, input_filename):
        return 'main.jar'

    def run(self, options, use_perf, source_filename):
        command = ['java', '-jar']

        if not self.jit:
            command += ['-Djava.compiler=None']

        command += [self.get_generated_executable(source_filename)]

        return Compiler.run(self, command, use_perf, source_filename)


class MOSML(Compiler):
    def __init__(self, executable=['mosmlc']):
        self.executable = executable

    def get_generated_executable(self, input_filename):
        return './a.out'

    def compile(self, filename, options):
        return Compiler.compile(self, self.executable + options + [filename],
                                filename)

    def run(self, options, use_perf, source_filename):
        command = [self.get_generated_executable(source_filename)]

        return Compiler.run(self, command, use_perf, source_filename)


class NumpyMachineDataWrapper(object):
    """ This is a wrapper that holds multiple instances
        of NumpyMachineData.  """
    def __init__(self, datas=[]):
        self.data = []

        # This is a dictionary of benchmark names.
        # Each name contains 'execution_time', 'compile_time', etc.
        # Each of those items is a list of lists.
        # More specifically, the format is:
        # {
        #    'benchmark': [
        #       {'execution_time': [
        #           {'machine': __, 'time': [...]}}}
        self.data_by_benchmark = {}

        for data in datas:
            self.add(data.to_numpy())

    def _init_benchmark_data(self, key):
        if key not in self.data_by_benchmark:
            self.data_by_benchmark[key] = {}
            self.data_by_benchmark[key]['execution_time'] = []
            self.data_by_benchmark[key]['compile_time'] = []

    def add(self, nmd):
        self.data.append(nmd)

        for key in nmd.run_times:
            self._init_benchmark_data(key)

            self.data_by_benchmark[key]['execution_time'].append({
                'machine': nmd.machine_name,
                'times': nmd.run_times[key]
            })

        for key in nmd.compile_times:
            self._init_benchmark_data(key)

            self.data_by_benchmark[key]['compile_time'].append({
                'machine': nmd.machine_name,
                'times': nmd.compile_times[key]
            })

    def data_for_benchmark(self, benchmark):
        """ This returns a list of all NumpyMachineDatas held
            that match the benchmark name.  """
        return self.data_by_benchmark[benchmark]


class NumpyMachineData(object):
    def __init__(self, machine_name):
        self.machine_name = machine_name
        self.run_times = {}
        self.compile_times = {}

    def add_run(self, test_name, times_list):
        self.run_times[test_name] = np.array(times_list)

    def add_compile(self, test_name, times_list):
        self.compile_times[test_name] = np.array(times_list)

    def to_numpy(self):
        """ This is an empty method so that NumpyMachineDataWrapper can
            accept lists of this type and of Data type.  """
        pass


class DataItem(object):
    """ This stores the data for individual runs. """
    def __init__(self):
        self.execution_passed = None
        self.execution_time = None
        self.benchmark_name = None
        self.compile_time = None
        self.code_size = None

        self.compile_failed = False
        self.run_failed = False

    def set_execution_time(self, time):
        self.execution_time = float(time)

    def get_execution_time(self):
        return self.execution_time

    def set_execution_pass(self, passed):
        self.execution_passed = passed

    def get_execution_pass(self):
        return self.execution_passed

    def set_benchmark_name(self, name):
        self.benchmark_name = name

    def get_benchmark_name(self):
        return self.benchmark_name

    def set_compile_time(self, compile_time):
        self.compile_time = compile_time

    def get_compile_time(self):
        return self.compile_time

    def set_run_failed(self, failed):
        self.run_failed = failed

    def get_run_failed(self):
        return self.run_failed

    def set_compile_failed(self, failed):
        self.compile_failed = failed

    def get_compile_failed(self):
        return self.run_failed

    def set_code_size(self, size):
        self.code_size = size

    def get_code_size(self):
        return self.code_size

    def all_defined(self):
        return (self.get_compile_failed() or
                self.get_run_failed()) or \
                self.execution_passed is not None and \
                self.execution_time is not None and \
                self.benchmark_name is not None and \
                self.code_size is not None and \
                self.compile_time is not None


class Data(object):
    def __init__(self, machine_name, project_version):
        self.lnt_items = []
        self.machine_name = machine_name
        self.project_version = project_version

    def add(self, item):
        # Require that all the fields of the DataItem
        # are defined.
        assert item.all_defined()
        self.lnt_items.append(item)

    def to_numpy(self):
        """ This function returns a list of numpy data items.  """
        dictionary = self.to_dictionary()

        data = NumpyMachineData(dictionary['machine']['name'])

        for test in dictionary['tests']:
            # Extract the compile times and the runtime separately.
            data.add_run(test['name'], test['execution_time'])
            data.add_compile(test['name'], test['compile_time'])

        return data

    def to_dictionary(self, starttime=datetime.datetime.now().isoformat()):
        """ This function returns a dictionary in the style the LNT
            expects. """
        tests_list = []

        results = {
            "format_version": "2",
            "machine": {
                "name": self.machine_name
            },
            "run": {
                "start_time": starttime,
                "end_time": datetime.datetime.now().isoformat(),
                "llvm_project_revision": self.project_version,
            },
            "tests": tests_list
        }

        for item in self.lnt_items:
            item_added = False
            test_failed = False

            if item.get_compile_failed() or item.get_run_failed():
                print "Benchmark " + item.get_benchmark_name() + " failed"
                test_failed = True

            run_failed = item.get_run_failed() or not item.get_execution_pass()

            for benchmark in tests_list:
                # Scan through the already added items.  If the item is already
                # there, then append to that.
                if benchmark['name'] == item.get_benchmark_name():
                    # Non-zero for a compile failure.  By adding onto this, we
                    # are guaranteed to preserve the non-zeroness of the
                    # benchmark.
                    benchmark['compile_status'] += \
                        1 if item.get_compile_failed() else 0
                    benchmark['execution_status'] += 1 if run_failed else 0

                    # Really, the code sizes should be the same.
                    benchmark['code_size'] = \
                        max(item.get_code_size(), benchmark['code_size'])

                    benchmark['execution_time'].append(
                            item.get_execution_time())
                    benchmark['compile_time'].append(
                            item.get_compile_time())

                    item_added = True

            if not item_added:
                # Then add this benchmark to the end of the list.
                tests_list.append({
                    'name': item.get_benchmark_name(),
                    'execution_time': [item.get_execution_time()],
                    'compile_time': [item.get_compile_time()],
                    'compile_status': 1 if item.get_compile_failed() else 0,
                    'execution_status': 1 if run_failed else 0,
                    'code_size': item.get_code_size()
                })

        return results

    def __str__(self):
        return json.dumps(self.to_dictionary(), sort_keys=True, indent=4)


def parse_output(output, compile_time, perf_directory, benchmark_id,
                 executable_file):
    """ Given the output of a program as defined in the README,
        extract that output into a dictionary as expected by LNT.  """
    lnt_data_item = DataItem()

    if compile_time == None:
        # The compile failed. Mark that and return.
        lnt_data_item.set_compile_failed(True)
        return lnt_data_item

    if output is None:
        # The run failed. Mark that and return
        lnt_data_item.set_run_failed(True)
        return lnt_data_item

    # We chose not to record this if the run failed.  We could do, but it
    # would be borderline useless.
    if executable_file is not None:
        lnt_data_item.set_code_size(os.path.getsize(executable_file))

    if perf_directory:
        # We copy the data item into the output folder.
        perf_file_name = os.path.join(perf_directory, benchmark_id) + '.perf'
        subprocess.check_output(['mv', 'data.perf', perf_file_name])

    # See the associated README.  That explains the expected
    # format of the output.
    lnt_data_item.set_compile_time(compile_time)

    for line in output.split('\n'):
        if line.strip(' ').startswith('Execution Time:'):
            lnt_data_item.set_execution_time(line.split(':')[1].strip(' '))
        elif line.strip(' ').startswith('Validation:'):
            was_validated = line.split(':')[1].strip(' ')

            if was_validated == 'pass':
                lnt_data_item.set_execution_pass(True)
            else:
                lnt_data_item.set_execution_pass(False)

                print 'Error: Benchmark failed validation'
                print 'Expected "pass", found', was_validated

        elif line:
            print 'Error: Unexpected line contents', line

    return lnt_data_item


def execute_benchmarks(compiler, benchmarks_list, compile_options,
                       runtime_options, runtime_perf_directory,
                       number, machine_name, project_version,
                       name_prefix):
    """ Take the list of benchmarks and the associated options
    and compile, run, and colate the data for each.

    Return that data as a list of dictionaries, where each dictionary
    is as LNT expects it in input.  """
    benchmark_data = Data(machine_name, project_version)

    for benchmark_folder in benchmarks_list:
        os.chdir('benchmarks/' + benchmark_folder)
        print "Running", benchmark_folder

        for i in range(number):
            print "Run number: ", str(i)
            benchmark_file = 'main.sml'
            compile_time = compiler.compile(benchmark_file, compile_options)
            output = \
                compiler.run(runtime_options,
                             runtime_perf_directory is not None,
                             benchmark_file)

            lnt_item = parse_output(output, compile_time,
                                    runtime_perf_directory,
                                    machine_name + benchmark_folder +
                                    '_' + str(i),
                                    compiler.get_generated_executable(
                                        benchmark_file))
            lnt_item.set_benchmark_name(name_prefix + benchmark_folder)

            benchmark_data.add(lnt_item)

        os.chdir('../..')

    return benchmark_data


def find_benchmarks(filter):
    """ This searches the current directory for all subfolders
    that are of the correct format for benchmarks (i.e. have a 'main.sml'
    file in them).  """

    benchmarks = []
    all_files = os.listdir('./benchmarks')

    if filter:
        all_files = fnmatch.filter(all_files, filter)

    for file in all_files:
        if os.path.isdir('benchmarks/' + file):
            benchmarks.append(file)

    return benchmarks


if __name__ == "__main__":
    parser = argparse.ArgumentParser(
            description='Run the benchmarks. Run '
            'each benchmark 5 times by default with one warmup run. The '
            'times are measured, and stored in "results.json", which is a '
            'file suitable for LNT import')

    parser.add_argument('--compile-options', dest='compile_options',
                        default='', action='store',
                        help='Options to pass during compilation')
    parser.add_argument('--runtime-options', dest='runtime_options',
                        default='', action='store',
                        help='Options to pass during runtime')
    parser.add_argument('--run-use-perf', dest='run_perf',
                        action='store', default=None,
                        help=('Use perf stat on the run of the benchmarks. '
                              'If so, a directory in which to store must '
                              'be provided as argument.'))
    parser.add_argument('--benchmarks', dest='benchmark_pattern',
                        action='store', default=None,
                        help=('only run benchmarks whose names match the'
                              'regex provided'))
    parser.add_argument('--runtime', dest='runtime',
                        action='store',
                        help=('The runtime environment to use'
                              ' (i.e. for javac this should be "java") '))
    parser.add_argument('--runs', dest='number_of_runs',
                        action='store', default=10, type=int,
                        help='The number of runs to do')
    parser.add_argument('--compiler', dest='compiler',
                        action='store', default=None,
                        help=('The executable to use. If mosml '
                              'are specified, no executable is needed. '))
    parser.add_argument('--use-jit', dest='use_jit',
                        action='store_true', default=False,
                        help=('If running the executable on the JVM, use '
                              'JIT compilation.'))
    parser.add_argument('--json-dump', dest='dump_file',
                        action='store', default='times.json',
                        help=('The dump file to use store the JSON in.'))
    parser.add_argument('--machine-name', dest='machine_name',
                        action='store', required=True,
                        help=('The machine name to use when creating records '
                              'of performance.'))
    parser.add_argument('--project-version', dest='project_version',
                        action='store', required=True, type=int,
                        help=('The version of the compiler used for ordering '
                              'of data.'))
    parser.add_argument('--name-prefix', dest='name_prefix',
                        action='store', default='',
                        help=('A prefix to put on the name of each test.'))

    compiler_group = parser.add_mutually_exclusive_group(required=True)

    compiler_group.add_argument('--mosml', dest='use_mosml',
                                action='store_true',
                                help='Use mosml as the compiler',
                                default=False)
    compiler_group.add_argument('--cmlc', dest='use_cmlc',
                                action='store_true',
                                help=('Use cmlc as the compiler'))

    args = parser.parse_args()
    compiler = None

    if args.use_mosml:
        compiler = MOSML()

    if args.use_cmlc:
        compiler = CMLC(executable=args.compiler.split(' '),
                        use_jit=args.use_jit)

    # The options for which compiler to use are mutually excludisve
    # and one is required. Do not expect this to happen.
    assert compiler is not None

    compile_options = []
    runtime_options = []

    if args.compile_options:
        compile_options = args.compile_options.split(' ')
        for index in range(0, len(compile_options)):
            if compile_options[index][0] == '-':
                raise Error("Argument error: Arguments passed with be prefixed"
                            "with - and -- automatically.  Found" + "'" +
                            compile_options[index] + "'")

            if len(compile_options[index]) == 1:
                compile_options[index] = '-' + compile_options[index]
            else:
                compile_options[index] = '--' + compile_options[index]

    if args.runtime_options:
        runtime_options = args.runtime_options.split(' ')

    starttime = datetime.datetime.now().isoformat()

    # Set the perf directory to an absolute path since the execution
    # takes place within various directories.
    full_run_perf = os.path.join(os.getcwd(), args.run_perf)

    # Now, build the benchmarks.
    benchmarks = find_benchmarks(args.benchmark_pattern)
    gathered_data = execute_benchmarks(compiler, benchmarks,
                                       compile_options,
                                       runtime_options,
                                       full_run_perf, args.number_of_runs,
                                       args.machine_name, args.project_version,
                                       args.name_prefix)

    print json.dumps(gathered_data.to_dictionary(starttime=starttime),
                     sort_keys=True, indent=4)
    with open(args.dump_file, 'w') as f:
        json.dump(gathered_data.to_dictionary(), f)
