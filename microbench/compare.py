#!/usr/bin/python

# This script takes a list of compilers and options
# It executes the benchmarks on each compiler-option set
# and then produces a whole dump of graphs for each.

import argparse
import matplotlib.pyplot as plot
import microrun
import numpy as np
import json


def execute_compilers(compilers, benchmark_list, number):
    """ This function takes a list of tuples.
        The tuples should be of the form:

            (executable, [options list])

        The executable is executed with each of the
        options list.  """
    gathered_data = []

    for compiler in compilers:
        assert len(compiler) is 4

        compiler_class = compiler[0]
        compile_options = compiler[1].split(' ')
        runtime_options = compiler[2].split(' ')
        name = compiler[3]

        gathered_data.append(
                microrun.execute_benchmarks(compiler_class, benchmark_list,
                                            compile_options, runtime_options,
                                            False, number, name, 0, ''))

    return gathered_data


def draw_box_plots(data, benchmark_name, title, ylabel, xlabel,
                   output_name=None, dict_access=None):
    if not output_name:
        output_name = 'bar_' + benchmark + '_'

    plottable_data = []
    tick_labels = []

    for datum in data:
        if 'machine' in datum:
            tick_labels.append(datum['machine'])

        if dict_access:
            datum = datum[dict_access]

        spread = [np.std(datum)]
        median = [np.median(datum)]
        high = [np.max(datum)]
        low = [np.min(datum)]

        plottable_data.append(np.concatenate((spread, median, high, low), 0))

    plot.boxplot(plottable_data, labels=tick_labels)

    plot.title(title)
    plot.ylabel(ylabel)
    plot.xlabel(xlabel)

    # TODO -- ENSURE SCALING AND CUTING OF AXES IS CORRECT.

    plot.savefig('./graphs/' + output_name + '.png')
    plot.cla()
    plot.clf()


def draw_bar_charts(data, benchmark_name, title, ylabel, xlabel,
                    bar_height_function, draw_error_bars,
                    dict_access=None, output_name=None):
    if not output_name:
        output_name = 'bar_' + benchmark_name + '_'

    error = []
    value = []
    tick_labels = []

    left_edges = []

    for datum in data:
        tick_label = "Tick not found"
        if 'machine' in datum:
            tick_label = datum['machine']
        tick_labels.append(tick_label)

        if dict_access:
            datum = datum[dict_access]

        error.append(np.std(datum))
        value.append(bar_height_function(datum))

        if left_edges:
            left_edges.append(left_edges[len(left_edges) - 1] + 0.2)
        else:
            left_edges.append(0.2)

    if draw_error_bars:
        error_bars = np.array(error)
    else:
        error_bars = None

    plot.bar(left_edges, np.array(value), yerr=error_bars, width=0.1,
             tick_label=tick_labels, align='center', color='red')

    plot.title(title)
    plot.ylabel(ylabel)
    plot.xlabel(xlabel)

    plot.autoscale()
    plot.savefig('./graphs/' + output_name + '.png')
    plot.cla()
    plot.clf()


def draw_all_graphs(numpy_data, benchmarks):
    """ Given a NumpyMachineDataWrapper and a list of benchmarks,
        plot all the graphs.  """

    for benchmark in benchmarks:
        datas = numpy_data.data_for_benchmark(benchmark)

        # Draw the box plots
        draw_box_plots(datas['execution_time'], benchmark,
                       'Execution Time of ' + benchmark,
                       'Seconds', 'Compiler',
                       output_name='box_execution_' + benchmark,
                       dict_access='times')
        draw_box_plots(datas['compile_time'], benchmark,
                       'Compile Time of ' + benchmark,
                       'Seconds', 'Compiler',
                       output_name='box_compile_' + benchmark,
                       dict_access='times')

        # Now draw all four sensible bar charts
        draw_bar_charts(datas['execution_time'], benchmark,
                        'Minimum Execution Time of ' + benchmark,
                        'Seconds', 'Compiler', np.min, False,
                        output_name='bar_min_execution_' + benchmark,
                        dict_access='times')
        draw_bar_charts(datas['execution_time'], benchmark,
                        'Median Execution Time of ' + benchmark,
                        'Seconds', 'Compiler', np.median, True,
                        output_name='bar_median_execution_' + benchmark,
                        dict_access='times')
        draw_bar_charts(datas['compile_time'], benchmark,
                        'Minimum Compile Time of ' + benchmark,
                        'Seconds', 'Compiler', np.min, False,
                        output_name='bar_min_compile_' + benchmark,
                        dict_access='times')
        draw_bar_charts(datas['compile_time'], benchmark,
                        'Median Compile Time of ' + benchmark,
                        'Seconds', 'Compiler', np.median, True,
                        dict_access='times',
                        output_name='bar_median_compile_' + benchmark)


if __name__ == "__main__":
    parser = argparse.ArgumentParser(description="""
    This is a tool for generating comparsion graphs  between benchmark
    runs.  """)

    parser.add_argument('--filter', action='store',
                        dest='filter', default=None,
                        help='Only run the benchmarks matching this filter')
    parser.add_argument('--mosml', action='append',
                        dest='mosml_options', default=[], nargs=3,
                        metavar=('compile_options', 'runtime_options', 'name'),
                        help="""Run MOSML with these options.  This may
                        be specified multiple times.  This argument takes
                        three arguments.  See --cmlc for more details""")
    parser.add_argument('--cmlc', action='append',
                        dest='cmlc_options', default=[], nargs=3,
                        metavar=('compile_options', 'runtime_options', 'name'),
                        help="""Run CMLC with these options.  This may
                        be specified multiple times.  This argument
                        exepcts three arguments.  The first specifies
                        the compile time arguments.  The second specifies
                        runtime arguments.  The last specifies the name
                        to use.  """)
    parser.add_argument('--cmlc-executable', action='store',
                        dest='cmlc_compiler', default='cmlc',
                        help="""The executable to use for CMLC""")
    parser.add_argument('--mosml-executable', action='store',
                        dest='mosml_compiler', default='mosmlc',
                        help="""The executable to use for MOSML""")
    parser.add_argument('--number', action='store',
                        type=int, dest='number', default=10,
                        help="""The number  of runs to consider.  """)

    args = parser.parse_args()

    benchmark_folders = microrun.find_benchmarks(args.filter)

    cmlc_compiler = microrun.CMLC(args.cmlc_compiler.split(' '))
    mosml_compiler = microrun.MOSML(args.mosml_compiler.split(' '))

    # Build up a list of all the compilers to use. These
    # Are lists of lists as specified above. We prepend the
    # compiler executable onto each one before use.
    compilers = []

    for item in args.cmlc_options:
        compilers.append([cmlc_compiler] + item)

    for item in args.mosml_options:
        compilers.append([mosml_compiler] + item)

    # This calls microrun.py to execute and time the various benchmarks.
    gathered_data = execute_compilers(compilers, benchmark_folders,
                                      args.number)

    # Now, draw ALL the graphs.
    numpy_arrays = microrun.NumpyMachineDataWrapper(gathered_data)

    draw_all_graphs(numpy_arrays, benchmark_folders)
