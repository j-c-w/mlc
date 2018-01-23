#!/usr/bin/python

# This is a script for running the testsuite.
# Use run_test.py --help for more information.

import argparse
import atexit
import fnmatch
import glob
import os
import re
import shutil
import subprocess
import sys


class Test(object):
    def __init__(self, build_from=None):
        self.scans = []
        self.options = []
        self.compile_should_fail = False
        self.should_run = False
        self.run_scan_regex = None

        if build_from:
            self.scans = list(build_from.scans)
            self.options = list(build_from.options)
            self.compile_should_fail = build_from.compile_should_fail
            self.should_run = build_from.should_run
            self.run_scan_regex = str(build_from.run_scan_regex)

    def __str__(self):
        return "scans = " + str(self.scans) + "\n" + \
            "options = " + str(self.options) + "\n" + \
            "compile_should_fail = " + str(self.compile_should_fail) + "\n" + \
            "should_run = " + str(self.should_run) + "\n" + \
            "run_scan_regex = " + str(self.run_scan_regex) + "\n"

    def add_scan(self, regex, dumpfile, times):
        self.scans.append((regex, dumpfile, times))

    def add_option(self, option):
        self.options.append(option)

    def set_compile_should_fail(self, fail):
        self.compile_should_fail = fail

    def file_for_affix(self, filename, affix):
        filename_only = os.path.basename(os.path.normpath(filename))
        dumpfiles = glob.glob(filename_only + "*." + affix)

        if len(dumpfiles) == 0:
            raise Exception("Dumpfile specified by " + affix +
                            " does not exist")
        if len(dumpfiles) != 1:
            raise Exception("Dumpfiles specified by affix " + affix + " are " +
                            " not unique. ")

        return dumpfiles[0]

    def set_should_run(self, should_run, run_scan_regex):
        self.should_run = should_run
        self.run_scan_regex = run_scan_regex

    def run_executable(self, name):
        # We have to remove the normal extension from this file:
        name = re.sub('\\.[^.]*$', '', name)
        jar_file = self.file_for_affix(name, 'jar')
        # -Xfuture ensures strictest possible checks on the integrity of the
        # jar files.  It can flag up potential errors with future JVM
        # implementations that are not yet user visible.
        return ['java', '-Xfuture', '-jar', jar_file]

    def execute(self, executable, file, full_filepath, additional_options):
        """ Returns true if there was an error or the test failed
            to run as expected.  """
        # First do the build
        print 'compiling'
        print 'file is', full_filepath

        # Scalop is not happy about duplicate flags. We therefore
        # only present each argument once
        deduplicated_options = list(set(self.options + additional_options))
        print 'with options', deduplicated_options

        return_value = \
            subprocess.call(executable.split(' ') + deduplicated_options +
                            [file])

        other_messages = []

        if return_value != 0:
            build_failed = True
            test_failed = not self.compile_should_fail
        else:
            # Now run:
            if self.should_run:
                try:
                    run_output = \
                        subprocess.check_output(self.run_executable(file))
                    if re.match(self.run_scan_regex, run_output):
                        # Then the match suceeded.
                        other_messages += ['PASS: Match of output in ' +
                                           full_filepath + ' passed']
                    else:
                        other_messages += ['FAIL: Match of output in ' +
                                           full_filepath + ' failed. ' +
                                           ' Looked for "' +
                                           self.run_scan_regex + '"']
                except subprocess.CalledProcessError as error:
                    # There was a runtime error
                    other_messages += ['FAIL: Runtime error in ' +
                                       full_filepath + 'error was ' +
                                       str(error)]
            build_failed = False
            test_failed = self.compile_should_fail

        if test_failed:
            return (test_failed, ['FAIL: Build of ' + full_filepath +
                                  ' failed.'] + other_messages)
        else:
            return (test_failed, ['PASS: Build of ' + full_filepath] +
                    other_messages)

    def run_scans(self, directory, filename):
        """ Filename points  to the original location of the
        test for the sake of the log message. 'source_name' points
        to the compiled source for the sake of scanner.  """
        print 'running scans in ', directory
        print 'for filename ', filename
        results = []

        for regex, dumpfile_affix, times in self.scans:
            dumpfile = self.file_for_affix(filename, dumpfile_affix)

            if not os.path.exists(dumpfile):
                results += ["FAIL: No dumpfile " + dumpfile +
                            " for filename " + filename]
                continue

            with open(dumpfile) as f:
                matches = re.findall(regex, ''.join(f.readlines()))

                if (len(matches) > 0 and times == -1) \
                        or len(matches) == times:
                    result = "PASS: "
                else:
                    result = "FAIL: "

                res_string = (result + dumpfile + " from " +
                              filename + " scanned for '" +
                              regex + "' ")

                if times > 0:
                    res_string += str(times) + " times"

                results.append(res_string)

        return results


def docstring():
    return """ This takes the filename and extracts the arguments and
        scanning passes needed for it.

        Each file should have either a 'compile' and/or a 'run' directive.
        There may be multiple compile directives.  If a run directive
        is specified, then the test will be run on every compilation.
        Scans will be executed on every compile.

        These should be of the form:

        (* t-compile: [options] *)
        (* t-fail *)

        A t-fail will mean that the test is marked as PASS when
        the file /fails/ to compile and FAIL when the file /successfully/
        compiles.

        In both cases, leave expected output blank to ignore it.

        Options should be a space  separated list as the arguments
        would appear as passed.

        The other directives are the 'scan' directives.

        These are used to scan the output for something matching.

        These are of the form:

        (* t-scan: [pattern] : dumpfilename *)
        (* t-scan-not: [pattern] : dumpfilename *)
        (* t-scan-times-N: [pattern] : dumpfilename *)

        Where in the last 'N' is some natural.
        'dumpfilename' is the name of the last part of the dumpfile,
        e.g. in test.sml.0.ast, dumpfilename should be 'ast'
        """


def find_tests(filter=None, root='.'):
    """ Taking a filter, list all the files in the subdirectory
        of this directory.  Return a list of all such files.
        If filter is set, then return only files that match that.
    """

    matches = []
    for path, dirname, filenames in os.walk(root):
        for filename in fnmatch.filter(filenames, '*.sml'):
            if not filter or re.match(filter, path + '/' + filename):
                matches.append(os.path.join(path, filename))

    return matches


def extract_information(filename, include_flags, include_scan):
    test_data = Test()
    compile_directives = []

    with open(filename) as f:
        lines = f.readlines()

        for line in lines:
            # Strip the comment markers on either side
            line = re.sub('\s*\(\*\s*', '', line)
            line = re.sub('\s*\*\)\s*', '', line)

            parts = line.split(':')

            # Strip the opening and closing parts of the comment.

            if line.startswith('t-compile:'):
                if include_flags:
                    compile_directives.append(parts[1])
                else:
                    compile_directives.append('')

            elif include_scan and line.startswith('t-run:'):
                test_data.set_should_run(True, parts[1].strip(' '))
            elif line.startswith('t-fail'):
                test_data.set_compile_should_fail(True)
            elif include_scan and line.startswith('t-scan:'):
                dumpfile = parts[2].strip(' ')
                regex = parts[1].strip(' ')

                test_data.add_scan(regex, dumpfile, -1)
            elif include_scan and line.startswith('t-scan-not:'):
                dumpfile = parts[2].strip(' ')
                regex = parts[1].strip(' ')

                test_data.add_scan(regex, dumpfile, 0)
            elif include_scan and line.startswith('t-scan-times-'):
                times = int(parts[0].split('-')[3].strip(' '))
                dumpfile = parts[2].strip(' ')
                regex = parts[1].strip(' ')

                test_data.add_scan(regex, dumpfile, times)

    # Now generate a list of tests from the compile directives:
    
    tests = []
    for compile in compile_directives:
        test = Test(test_data)
        for option in compile.split(' '):
            if option:
                test.add_option(option.strip(' '))
        tests.append(test)

    return tests


def run_test(filename, executable, options, include_flags, include_scan):
    """ Given filename as some tests, this extracts
        the information we need (arguments, scan targets),
        runs  the test and checks the arguments.  """
    name_only = os.path.basename(filename)
    test_data = extract_information(filename, include_flags, include_scan)
    failure_data = []

    for test in test_data:
        # Create the execute directory
        os.mkdir('execute')
        os.chdir('execute')

        # Copy the test into the temp folder before executing.
        print filename
        shutil.copyfile('../' + filename, name_only)

        # Now, do the build/run and FAIL if there were any errors.
        (failed, test_data) = \
            test.execute(executable, name_only, filename, options)
        failure_data += test_data
        if failed:
            os.chdir('..')
            # Clear the execute directory
            shutil.rmtree('execute')
            continue

        # Then, do the scan of the dumps:
        failure_data += test.run_scans('./execute',  filename)

        os.chdir('..')
        # Clear the execute directory
        shutil.rmtree('execute')

    return failure_data


def dump_result_data(results, dumpfile):
    with open(dumpfile, "w") as f:
        for line in results:
            f.write(line + '\n')


def run_all(filenames, dumpfile, executable, options, use_flags, run_scans):
    results = []

    print 'dump is', dumpfile

    for filename in filenames:
        print 'executing', filename
        # Delete the execution folder if it exists.
        if os.path.exists('execute'):
            # There was probably a failure in a previous test run.
            # Delete it.
            shutil.rmtree('execute')
        results += \
            run_test(filename, executable, options, use_flags, run_scans)

    dump_result_data(results, dumpfile)


def cleanup():
    """ Delete the directory for testing on exit to keep it clean
        for the next execution.  """

    if os.path.exists('execute'):
        shutil.rmtree('execute')


if __name__ == "__main__":
    atexit.register(cleanup)

    parser = argparse.ArgumentParser(description='Run the testsuite\n\n\n' +
                                                 docstring())

    parser.add_argument('--filter', dest='regex_filter', action='store',
                        default=None, help=('Only run tests whose name matches'
                                            ' the regex passed'))
    parser.add_argument('--dir', dest='root', action='store',
                        default='.', help=('The  home directory to start'
                                           ' looking for tests in. "." by'
                                           '  default.'))
    parser.add_argument('--executable', dest='executable', action='store',
                        required=True, help=('The compiler executabe to use'
                                             ' to run these tests'))
    parser.add_argument('--output', dest='output_file', action='store',
                        default='test.res', help=('Output file to dump'
                                                  ' test results into'))
    parser.add_argument('--options', dest='options', action='store',
                        default='', help=('Options to set on every'
                                          ' compilation instance. '))
    parser.add_argument('--no-flags', dest='no_flags', action='store_true',
                        default=False, help=('Ignore flags specified by tests.'
                                             ' For running the testsuite with'
                                             ' other compilers.  See'
                                             ' --no-build-scan.  Many tests'
                                             ' will likely fail if it is not'
                                             ' specified in addition.'))
    parser.add_argument('--no-scan', dest='no_scan', action='store_true',
                        default=False, help=('Do not scan dumpfiles. '
                                             'Usually used in conjunction '
                                             ' with --no-flags.'))
    args = parser.parse_args()

    print "Executable is", args.executable
    if args.options:
        additional_options = args.options.split(' ')
    else:
        additional_options = []

    tests = find_tests(args.regex_filter, args.root)
    run_all(tests, args.output_file, args.executable, additional_options,
            not args.no_flags, not args.no_scan)
