#!/usr/bin/python

from matplotlib import pyplot
import argparse
import graph
import json
import matplotlib

OPTS = ['tail_elim', 't_inline', 'pre_lower_simplify', 'copy_prop',
        'simplify',  'byte_dce', 'peephole']


def sum_fields(data, runs, opts):
    times = []

    for key in data:
        for opt in opts:
            if key.startswith(opt):
                times.append(data[key])

    run_sum = []

    for i in range(runs):
        run_sum.append(0)
        for time in times:
            run_sum[i] += time[i]

    return run_sum


def sum_times_between(data, runs, passfrom, passto, include_opts=False):
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
        if key != 'total' and key != 'subprocess_times':
            if not include_opts or key not in OPTS:
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


def sum_ast_times_from(data, runs, include_opts):
    # Get the first and last AST pass.
    return sum_times_between(data, runs, 'input', 'lower_ast',
                             include_opts=include_opts)


def sum_tir_times_from(data, runs, include_opts):
    return sum_times_between(data, runs, 'lower_ast', 'lower_tir',
                             include_opts=include_opts)


def sum_byteR_times_from(data, runs, include_opts):
    return sum_times_between(data, runs, 'lower_tir', 'output',
                             include_opts=include_opts)


def sum_opt_times(data, runs):
    return sum_fields(data, runs, OPTS)


def gen_title_for(compile_pass, benchmark):
    # TODO -- Expect to insert custom titles here.
    if benchmark == "nested_lets":
        benchmark_item = "nested let satements"
    elif benchmark == "type_blowup":
        benchmark_item = "log2(type variables)"
    else:
        benchmark_item = benchmark

    if compile_pass:
        return "Time taken to execute pass " + compile_pass + \
                " against number of " + benchmark_item
    else:
        return "Compilation Time vs Number of " + benchmark_item


def gen_x_label_for(compile_pass, benchmark):
    # Expect to insert custom x-labels here.
    if benchmark == 'vals':
        return "Number of 'val' declarations"
    elif benchmark == 'applications':
        return "Number of function applications"
    elif benchmark == 'functions':
        return "Number of 'fun' declarations"
    elif benchmark == 'nested_lets':
        return "Number of nested 'let' declarations"
    elif benchmark == 'type_blowup':
        return "log2(Number of type variables)"
    elif benchmark == 'expressions':
        return "Number of expressions"
    raise Exception("Need to insert a name for benchmark " + benchmark)


def gen_y_lim_for(compile_pass, benchmark):
    if benchmark == 'vals':
        return 900
    elif benchmark == 'applications':
        return 2100
    elif benchmark == 'functions':
        return 1500
    elif benchmark == 'nested_lets':
        return 1500
    elif benchmark == 'type_blowup':
        return 2000
    elif benchmark == 'expressions':
        return 2400
    raise Exception("Need to insert a name for benchmark " + benchmark)


def gen_legend_string(compile_pass, benchmark):
    return compile_pass


def generic_compile_time_graph(axis_size, run_data, include_opts):
    # For this graph, split into TIR, byteR and AST.
    number = 4 if include_opts else 3
    runs = run_data['runs']
    x_data = []
    y_data = []
    errors = []

    for i in range(axis_size):
        x_data.append(i)

        y_data_dict = run_data[str(i) + '.sml']

        ast_times = \
            sum_ast_times_from(y_data_dict, runs,
                               include_opts=include_opts)
        tir_times = \
            sum_tir_times_from(y_data_dict, runs,
                               include_opts=include_opts)
        if include_opts:
            opt_times = sum_opt_times(y_data_dict, runs)

        byteR_times = \
            sum_byteR_times_from(y_data_dict, runs,
                                 include_opts=include_opts)

        if include_opts:
            tuples = [(ast_times[i], tir_times[i] + ast_times[i],
                       ast_times[i] + tir_times[i] + opt_times[i],
                       ast_times[i] + tir_times[i] + opt_times[i] +
                       byteR_times[i])
                      for i in range(runs)]
        else:
            tuples = [(ast_times[i], tir_times[i] + ast_times[i],
                       byteR_times[i] + ast_times[i] + tir_times[i])
                      for i in range(runs)]

        if runs == 1:
            selected_tuple = tuples[0]
            errors = None
        else:
            # Do the min max selection by overall time.
            def select(x):
                return x[3 if include_opts else 2]

            def averager(tuple):
                s0 = 0.0
                s1 = 0.0
                s2 = 0.0
                s3 = 0.0

                if include_opts:
                    for (a, b, c, d) in tuple:
                        s0 += a
                        s1 += b
                        s2 += c
                        s3 += d
                else:
                    for (a, b, c) in tuple:
                        s0 += a
                        s1 += b
                        s2 += c

                n = len(tuple)
                if include_opts:
                    return (s0 / n, s1 / n, s2 / n, s3 / n)
                else:
                    return (s0 / n, s1 / n, s2 / n)

            (min_err, max_err, selected_tuple) = \
                graph.generate_min_max_median(tuples,
                                              narrowing_function=select,
                                              averaging_function=averager,
                                              delete_min_max=3)
            errors.append((min_err, max_err))

        y_data.append(selected_tuple)

    if include_opts:
        labels = ["Time spent in AST Representation",
                  "Time spent in TIR Representation",
                  "Time spent Optimising",
                  "Time spent in ByteR Representation"]
    else:
        labels = ["Time spent in AST Representation",
                  "Time spent in TIR Representation",
                  "Time spent in ByteR Representation"]

    fig = graph.draw_stacked_line(number, x_data, y_data, errors,
                                  y_label="Compile Time (ms)",
                                  x_label=gen_x_label_for(None, benchmark),
                                  title=(gen_title_for(None, benchmark)),
                                  legend=labels,
                                  ylim_max=gen_y_lim_for(None, benchmark))
    fig.show()

    if include_opts:
        opts_string = "_with_opts_"
    else:
        opts_string = ""

    graph.save_to(fig, benchmark + opts_string + run_data['name'] +
                  '_compile_time.eps')


def per_pass_compile_times(axis_size, run_data, include_opts):
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
                                              delete_min_max=3)

            errors.append((min_err, max_err))
            y_data.append(value)
            x_data.append(i)

        fig = graph.draw_line(x_data, y_data, error_bars=errors,
                              x_label=gen_x_label_for(compile_pass, benchmark),
                              y_label="Time (ms)",
                              title=gen_title_for(compile_pass, benchmark))

        if include_opts:
            no_opts = "_with_opts_"
        else:
            no_opts = ""

        graph.save_to(fig, benchmark + no_opts + run_data['name'] + '_' +
                      compile_pass + '.eps')


if __name__ == "__main__":
    # Given an input file as an argument, output all the expented graphs.
    parser = \
        argparse.ArgumentParser("A tool for drawing compilation time "
                                "benchmarks")

    parser.add_argument('input_file', help=("Input JSON file as created by "
                                            "the benchmarking scripts"))
    parser.add_argument('--nohold', action='store_true', default=False,
                        help="Don't display the figure on the screen.")
    parser.add_argument('--with-opts', action='store_true', default=False,
                        dest='include_opts',
                        help='Highlight time spent doing optimizations')

    args = parser.parse_args()

    # Load in the JSON file:
    with open(args.input_file) as f:
        data = json.load(f)

    # Setup the font sizes
    SMALL_SIZE = 7
    MEDIUM_SIZE = 11
    LARGE_SIZE = 13

    matplotlib.rc('font', size=LARGE_SIZE)
    matplotlib.rc('axes', titlesize=LARGE_SIZE)
    matplotlib.rc('axes', labelsize=LARGE_SIZE)
    matplotlib.rc('xtick', labelsize=MEDIUM_SIZE)
    matplotlib.rc('ytick', labelsize=MEDIUM_SIZE)
    matplotlib.rc('legend', fontsize=MEDIUM_SIZE)
    matplotlib.rc('figure', titlesize=LARGE_SIZE)

    # Draw graphs:
    for benchmark in data:
        print "Drawing graph for benchmark", benchmark
        run_data = data[benchmark]

        # This is the number of times we will divide the data.
        axis_size = run_data['number']
        generic_compile_time_graph(axis_size, run_data, args.include_opts)
        per_pass_compile_times(axis_size, run_data, args.include_opts)

        if not args.nohold:
            pyplot.show(True)
