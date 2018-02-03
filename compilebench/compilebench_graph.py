#!/usr/bin/python

from matplotlib import pyplot
import argparse
import graph
import json


def sum_times_between(data, runs, passfrom, passto):
    start_no = None
    end_no = None

    for key in data:
        if key.startswith(passfrom):
            start_no = int(key[len(passfrom) + 1:])

        if key.startswith(passto):
            end_no = int(key[len(passto) + 1:])

    # Then get every key within that range and sum it.
    times = []

    for key in data:
        if key != 'total':
            pass_no = int(key[key.find('.') + 1:])
            if start_no < pass_no and end_no >= pass_no:
                times.append(data[key])

    # This is now a list of the form:
    # [run0: [lex_time, parse_time, ...]]
    # Sum up the per_pass values to get a single
    # number for the whole range
    run_sum = []

    for i in range(runs):
        run_sum.append(0)
        for time in times:
            run_sum[i] += time[i]

    return run_sum


def sum_ast_times_from(data, runs):
    # Get the first and last AST pass.
    return sum_times_between(data, runs, 'input', 'lower_ast')


def sum_tir_times_from(data, runs):
    return sum_times_between(data, runs, 'lower_ast', 'lower_tir')


def sum_byteR_times_from(data, runs):
    return sum_times_between(data, runs, 'lower_tir', 'output')


def gen_title_for(compile_pass, benchmark):
    # TODO -- Expect to insert custom titles here.
    return "Time taken to execute pass " + compile_pass + \
            " against number of " + benchmark


def gen_x_label_for(compile_pass, benchmark):
    # Expect to insert custom x-labels here.
    if benchmark == 'vals':
        return "Number of 'val' declarations"
    raise Exception("Need to insert a name for benchmark " + benchmark)


def gen_legend_string(compile_pass, benchmark):
    return compile_pass


def generic_compile_time_graph(axis_size, run_data):
    # For this graph, split into TIR, byteR and AST.
    number = 3
    runs = run_data['runs']
    x_data = []
    y_data = []
    errors = []

    for i in range(axis_size):
        x_data.append(i)

        y_data_dict = run_data[str(i) + '.sml']

        ast_times = sum_ast_times_from(y_data_dict, runs)
        tir_times = sum_tir_times_from(y_data_dict, runs)
        byteR_times = sum_byteR_times_from(y_data_dict, runs)

        tuples = [(ast_times[i], tir_times[i] + ast_times[i],
                   byteR_times[i] + ast_times[i] + tir_times[i])
                  for i in range(runs)]

        if runs == 1:
            selected_tuple = tuples[0]
            errors = None
        else:
            # Do the min max selection by overall time.
            def select(x):
                return x[2]

            def averager(tuple):
                s0 = 0.0
                s1 = 0.0
                s2 = 0.0

                for (x, y, z) in tuple:
                    s0 += x
                    s1 += y
                    s2 += z

                n = len(tuple)
                return (s0 / n, s1 / n, s2 / n)

            (min_err, max_err, selected_tuple) = \
                graph.generate_min_max_median(tuples,
                                              narrowing_function=select,
                                              averaging_function=averager,
                                              delete_min_max=True)
            errors.append((min_err, max_err))

        y_data.append(selected_tuple)

    fig = graph.draw_stacked_line(number, x_data, y_data, errors,
                                  y_label="Compile Time (ms)",
                                  x_label=gen_x_label_for(None, benchmark),
                                  title=("Compile Times for " + benchmark),
                                  legend=["Time spent in AST Representation",
                                          "Time spent in TIR Representation",
                                          "Time spent in ByteR " +
                                          "Representation"])
    fig.show()
    graph.save_to(fig, benchmark + run_data['name'] + '_compile_time.eps')


# Given an input file as an argument, output all the expented graphs.
parser = \
    argparse.ArgumentParser("A tool for drawing compilation time benchmarks")

parser.add_argument('input_file', help=("Input JSON file as created by the "
                                        "benchmarking scripts"))
parser.add_argument('--nohold', action='store_true', default=False,
                    help="Don't display the figure on the screen.")

args = parser.parse_args()

# Load in the JSON file:
with open(args.input_file) as f:
    data = json.load(f)

# Draw graphs:
for benchmark in data:
    print "Drawing graph for benchmark", benchmark
    run_data = data[benchmark]

    # This is the number of times we will divide the data.
    axis_size = run_data['number']
    generic_compile_time_graph(axis_size, run_data)

    # Also plot a graph for each specific pass.

    for compile_pass in run_data['0.sml']:
        errors = []
        y_data = []
        x_data = []

        for i in range(axis_size):
            # Get the data for the ith file:
            ith = run_data[str(i) + '.sml']

            (min_err, max_err, value) = \
                graph.generate_min_max_median(ith[compile_pass],
                                              delete_min_max=True)

            errors.append((min_err, max_err))
            y_data.append(value)
            x_data.append(i)

        fig = graph.draw_line(x_data, y_data, error_bars=errors,
                              x_label=gen_x_label_for(compile_pass, benchmark),
                              y_label="Time (ms)",
                              title=gen_title_for(compile_pass, benchmark),
                              legend=[gen_legend_string(compile_pass,
                                                        benchmark)])

        graph.save_to(fig, benchmark + run_data['name'] + '_' +
                      compile_pass + '.eps')

    if not args.nohold:
        pyplot.show(True)
