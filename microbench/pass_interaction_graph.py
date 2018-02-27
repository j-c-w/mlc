#!/usr/bin/python

import argparse
import copy
import glob
import numpy
import os
import perf_parser


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
for Mandelbrot without JIT}" offset 2
    set palette defined (-30 'red', 0 'white', 3000 'green')
        """
    else:
        color_settings = """
    set title "{/*1.3 Speedup from running pairs of optimizations \
for Mandelbrot}" offset 2
    set cbrange [-80:80]
    set palette defined (-300 'red', 0 'white', 300 'green')
        """

    # Now build the GNUPlot code.
    return """$map3 << EOD
    """ + "\n".join(row_data) + """
EOD

    set xlabel '{/*1.4 Additional Optimization}' offset 2
    set ylabel '{/*1.4 Base Optimization}' offset -2
    set palette model RGB
    """ + color_settings + """
    set cblabel "{/*0.9 Difference in execution time with addtional \
optimization (ms)}" offset 2
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


def get_relevant_data_from(data, baseline_data_pass1, baseline_data_pass2):
    """ Given a dictionary of perf data, pick some data from this
        dictionary and return it.  Each entry in this data dictionary
        is a list, and will be indexed by the perf name.

        Baseline data is the data for this pass on it's own.  """

    clock_times = data['cpu-clock']

    clock_times.remove(min(clock_times))
    clock_times.remove(max(clock_times))

    baseline_times_1 = baseline_data_pass1['cpu-clock']

    baseline_times_1.remove(min(baseline_times_1))
    baseline_times_1.remove(max(baseline_times_1))

    baseline_times_2 = baseline_data_pass2['cpu-clock']

    baseline_times_2.remove(min(baseline_times_2))
    baseline_times_2.remove(max(baseline_times_2))

    return min(numpy.mean(baseline_times_1), numpy.mean(baseline_times_2)) \
        - numpy.mean(clock_times)


def draw(args, data_folder):
    # Get all the perf files:
    files = glob.glob(data_folder + "/*_0.perf")

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
        targ_col = record_name.split("_")[1][:-len("mandelbrot")]

        # Add the datat to the record of it all
        full_data[targ_row][targ_col] = data

    # Extra loop required so that we know the baseliine data is set.
    for file in files:
        record_name = file[len(data_folder) + 1:]
        targ_row = record_name.split("_")[0]
        targ_col = record_name.split("_")[1][:-len("mandelbrot")]

        # Now, we can narrow the data down.
        rows[targ_row][targ_col] = \
            get_relevant_data_from(
                    # Deep copy because this method is allowed to change
                    # the dictionaries.
                    copy.deepcopy((full_data[targ_row][targ_col])),
                    # Repeat the targ_row twice here as a
                    # baseline.
                    copy.deepcopy((full_data[targ_row][targ_row])),
                    copy.deepcopy((full_data[targ_col][targ_col])))

    # Now, draw the row
    row_lists = ["," + ",".join(sorted(col_headers))]
    for targ_row in sorted(col_headers):
        row_data = [targ_row]

        for item in sorted(rows[targ_row]):
            row_data.append(str(rows[targ_row][item]))

        row_lists.append(",".join(row_data))

    # Now, get the graph drawing code:
    gnu_code = gen_gnuplot(args, row_lists)

    with open('mandelbrot_heatmap.gnu', 'w') as f:
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
