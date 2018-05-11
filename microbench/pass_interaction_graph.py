#!/usr/bin/python

import argparse
import copy
import glob
import numpy
import os
import perf_parser

BENCHMARK = 'matrix_multiplication'


def usage():
    return """
    This script draws graphs for data from the pass_interaction.sh
    script.  It uses the standard CMLC graphing library to do this.

    The data is extracted from perf files using the perf_parse file.

    This script produces a GNUPlot file.  The GNUPlot file is then compiled.
    """


def gen_gnuplot(args, row_data):
    # We need different formatting if this has been done with and without
    # the JIT.
    if args.without_jit_titles:
        color_settings = """
    set title "{/*1 Speedup from running pairs of optimizations \
for Matrix Multiplication without JIT}" offset 2
    set palette defined (-3000 'red', 0 'white', 3000 'blue')
        """
    else:
        color_settings = """
    set title "{/*1.3 Store miss rate change from running pairs of \
optimizations for Matrix Multiplication}" offset 2
    set palette defined (-300 'blue', 0 'white', 300 'red')
        """

    # Now build the GNUPlot code.
    return """$map3 << EOD
    """ + "\n".join(row_data) + """
EOD

    set xlabel '{/*1.4 Additional Optimization}' offset 2
    set ylabel '{/*1.4 Base Optimization}' offset -2
    set palette model RGB
    """ + color_settings + """
    set cblabel "{/*0.9 Addtional speedup when combining \
optimisations (ms)}" offset 2
    set datafile separator comma
    set terminal eps
    set output 'pass_interaction.eps'
    plot '$map3' matrix rowheaders columnheaders using 1:2:3 with image
    set terminal x11
    set output
    replot
    set datafile separator
    pause -1
    """


def get_relevant_data_from(data, baseline_data_pass1, no_opts_times,
                           baseline_data_pass2=None):
    """ Given a dictionary of perf data, pick some data from this
        dictionary and return it.  Each entry in this data dictionary
        is a list, and will be indexed by the perf name.

        Baseline data is the data for this pass on it's own.  """

    field = 'cpu-clock'
    field2 = 'L1-dcache-load-misses'

    def op((e1, e2)):
        return e1

    clock_times = foreach_do(data[field], data[field2], op)

    clock_times.remove(min(clock_times))
    clock_times.remove(max(clock_times))

    baseline_times_1 = \
        foreach_do(baseline_data_pass1[field], baseline_data_pass1[field2], op)

    if baseline_data_pass2:
        baseline_times_2 = \
            foreach_do(baseline_data_pass2[field],
                       baseline_data_pass2[field2], op)
    else:
        baseline_times_2 = None

    no_opts_time = \
        numpy.median(foreach_do(no_opts_times[field],
                                no_opts_times[field2], op))

    if baseline_times_2:
        return (no_opts_time - numpy.median(clock_times)) - \
               (no_opts_time - numpy.median(baseline_times_1) +
                no_opts_time - numpy.median(baseline_times_2))
    else:
        return (no_opts_time - numpy.median(clock_times)) - \
               (no_opts_time - numpy.median(baseline_times_1))


def foreach_do(list1, list2, fun):
    return map(fun, zip(list1, list2))


def draw(args, data_folder):
    # Get all the perf files:
    files = glob.glob(data_folder + "/*_0.perf")
    # The no_opts files must be processed specially.
    files = \
        [file for file in files
         if not os.path.basename(file).startswith("no_opts")]

    full_data = {}
    rows = {}
    col_headers = {}
    for file in files:
        # Get the column headers and row headers.
        record_name = file[len(data_folder) + 1:]
        rows[record_name.split("_")[0]] = {}
        full_data[record_name.split("_")[0]] = {}
        col_headers[record_name.split("_")[0]] = None

    for file in files:
        # Get the part without the number extension:
        file = file[:-len("0.perf")]

        # And get all the data from that file.
        data = perf_parser.parse_perf_files(file)

        # Now, note the row and col it belongs in.
        record_name = file[len(data_folder) + 1:]
        targ_row = record_name.split("_")[0]
        targ_col = record_name.split("_")[1][:-len(BENCHMARK.split('_')[0])]

        # Add the datat to the record of it all
        full_data[targ_row][targ_col] = data

    no_opts_data = \
        perf_parser.parse_perf_files(data_folder + "/no_opts" +
                                     BENCHMARK + "_")

    # Extra loop required so that we know the baseliine data is set.
    for file in files:
        record_name = file[len(data_folder) + 1:]
        targ_row = record_name.split("_")[0]
        targ_col = record_name.split("_")[1][:-len(BENCHMARK.split('_')[0])]

        # Now, we can narrow the data down.
        if targ_row in rows and targ_row in full_data and \
                targ_col in full_data[targ_row]:
            if targ_row == targ_col:
                rows[targ_row][targ_col] = \
                    get_relevant_data_from(
                            copy.deepcopy((full_data[targ_row][targ_col])),
                            copy.deepcopy((full_data[targ_row][targ_col])),
                            no_opts_data)
            else:
                rows[targ_row][targ_col] = \
                    get_relevant_data_from(
                            # Deep copy because this method is allowed to
                            # change the dictionaries.
                            copy.deepcopy((full_data[targ_row][targ_col])),
                            # Repeat the targ_row twice here as a
                            # baseline.
                            copy.deepcopy((full_data[targ_row][targ_row])),
                            no_opts_data,
                            copy.deepcopy((full_data[targ_col][targ_col])))
        else:
            print "Warning: no data for pair ", targ_row, targ_col

    # Now, draw the row
    row_lists = ["," + ",".join(sorted(col_headers))]
    for targ_row in sorted(col_headers):
        row_data = [targ_row]

        for item in sorted(rows[targ_row]):
            row_data.append(str(rows[targ_row][item]))

        row_lists.append(",".join(row_data))

    # Now, get the graph drawing code:
    gnu_code = gen_gnuplot(args, row_lists)

    with open(BENCHMARK + '_heatmap.gnu', 'w') as f:
        f.write(gnu_code)


if __name__ == "__main__":
    parser = argparse.ArgumentParser(description=usage())

    parser.add_argument('--no-hold', action='store_true',
                        dest='no_hold', default=False,
                        help='Do not hold the graph before exiting. ')
    parser.add_argument('data_folder', action='store',
                        help=('Specify the folder in which the data is '
                              'gathered.'))
    parser.add_argument('--without-jit-titles', action='store_true',
                        dest='without_jit_titles', default=False,
                        help=("Title the graph as if JIT had not been used"))

    args = parser.parse_args()

    if not os.path.exists(args.data_folder):
        raise Exception("Folder " + args.data_folder + " does not exist")
    draw(args, args.data_folder)
