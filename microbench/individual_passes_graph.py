import argparse
import graph
import numpy as np
import perf_parser

OPTS = ['copyprop', 'dce', 'inline', 'no_opts', 'peephole', 'simplify', 'tce']
OPT_NAMES = \
    {'copyprop': 'copyprop', 'dce': 'dse', 'inline': 'inline',
        'no_opts': 'no_opts', 'peephole': 'peephole', 'simplify': 'simplify',
        'tce': 'tce'}
BENCHMARKS = ['alignment', 'dft', 'inlining', 'knuth_bendix', 'life',
              'mandelbrot', 'matrix_multiplication', 'tak', 'utils']

FIELD = ['cpu-clock', 'cpu-clock', 'cpu-clock', 'L1-dcache-loads',
         'L1-dcache-load-misses', 'cpu-cycles', 'instructions',
         'branch-instructions', 'branch-misses']
FIELD2 = [None, None, None, None, 'L1-dcache-loads', None, 'cpu-cycles',
          None, 'branch-instructions']
NUM_TO_DROP = [3, 5, 0, 0, 0, 0, 0, 0, 0]
METRIC_NAME = ['Execution Time (ms)',
               'Relative Execution Time (Lower Means Speedup)',
               'Relative Execution Time (Lower Means Speedup)',
               'Relative Number of Cache References',
               'Relative Cache Miss Rate',
               '(Relative) Clock Cycles',
               '(Relative) Instructions per Clock Cycle',
               '(Relative) Branch Instructions',
               '(Relative) Branch Predictor Miss Rate']
NORMALIZE = [False, True, True, True, True, True, True, True, True]
WITH_JIT = [True, True, False, False, False, False, False, False, False]
OUTPUT_FILES = ['execution_time_individual_passes.eps',
                'relative_execution_time_individual_passes.eps',
                'relative_execution_time_individual_passes_no_jit.eps',
                'relative_cache_refs.eps',
                'relative_cache_misses.eps',
                'relative_clock_cycles.eps',
                'relative_cpi.eps',
                'relative_branch_instrs.eps',
                'relative_branch_miss_rate.eps']
TITLES = ['Execution Time of Benchmarks with Individual Optimisations Enabled',
          'Execution Time of Benchmarks with Individual Optimisations '
          ' Enabled \nRelative to an Unoptimised Run',
          'Execution Time of Benchmarks with Individual Optimisations '
          ' Enabled \nRelative to an Unoptimised Run (No JIT)',
          'Number of L1 Data Cache Loads Relative '
          'to an Unoptimised Run (No JIT)',
          'L1 Data Cache Load Miss Rate Relative '
          'to an Unoptimised Run (No JIT)',
          'Clock Cycles per Run Relative to an Unoptimised Run (No JIT)',
          'Instructions per Clock Cycle Relative to an Unoptimised Run '
          '(No JIT)',
          'Branch Instructions per Run Relative to an Unoptimised Run '
          '(No JIT)',
          'Branch Predictor Miss Rate Relative to an Unoptimised Run']
FIG_SIZE = [None, (8, 6),  (8, 6), (8, 6), (8, 6), (8, 6), (8, 6),
            (8, 6), (8, 6)]
TOP_PADDING = [None, None, None, None, None, None, None, 0.025, None]


def usage():
    return """
    This is a script for drawing graphs based on the output of
    individual passes.
    """


if __name__ == "__main__":
    parser = argparse.ArgumentParser(description=usage())
    # Get the data from the perf files:

    parser.add_argument("directory", help="Directory with perf files")
    parser.add_argument("--nohold", action="store_true", dest="nohold",
                        default=False, help="Do not hold graphs")
    args = parser.parse_args()

    for graph_no in range(len(FIELD) - 1, -1, -1):
        # We group the results by benchmark.
        groups = {}
        errors = {}

        for opt in OPTS:
            groups[opt] = []
            errors[opt] = []

        # Get the data:
        for opt in OPTS:
            for benchmark in BENCHMARKS:
                if not WITH_JIT[graph_no]:
                    data = \
                        perf_parser.parse_perf_files(args.directory + "/" +
                                                     opt + '-no-jit' +
                                                     benchmark + "_")
                    normalize_data = \
                        perf_parser.parse_perf_files(args.directory + "/" +
                                                     "no_opts-no-jit" +
                                                     benchmark + "_")
                else:
                    data = perf_parser.parse_perf_files(args.directory + "/" +
                                                        opt + benchmark + "_")

                    normalize_data = \
                        perf_parser.parse_perf_files(args.directory + "/" +
                                                     "no_opts" + benchmark +
                                                     "_")

                field = data[FIELD[graph_no]][:]

                # Drop the highest and lowest elements.
                for i in range(NUM_TO_DROP[graph_no]):
                    field.remove(min(field))
                    field.remove(max(field))

                if NORMALIZE[graph_no]:
                    field_normalization = normalize_data[FIELD[graph_no]][:]

                    for i in range(NUM_TO_DROP[graph_no]):
                        field_normalization.remove(min(field_normalization))
                        field_normalization.remove(max(field_normalization))

                    field_normalization_err = \
                        (min(field_normalization), max(field_normalization))
                    field_normalization = np.median(field_normalization)

                if not FIELD2[graph_no]:
                    # Average the data.
                    group_value = (np.median(field))
                    error_values = (min(field), max(field))
                else:
                    field2 = data[FIELD2[graph_no]][:]
                    for i in range(NUM_TO_DROP[graph_no]):
                        field2.remove(min(field2))
                        field2.remove(max(field2))

                    group_value = (np.median(field) / np.median(field2))
                    error_values = ((min(field) / max(field2),
                                     max(field) / min(field2)))

                    if NORMALIZE[graph_no]:
                        field_normalization_2 = normalize_data[FIELD2[graph_no]]

                        for i in range(NUM_TO_DROP[graph_no]):
                            field_normalization_2.remove(
                                    min(field_normalization_2))
                            field_normalization_2.remove(
                                    max(field_normalization_2))

                        field_normalization = \
                            (field_normalization /
                             np.median(field_normalization_2))
                        field_normalization_err = (field_normalization_err[0] /
                                                   max(field_normalization_2),
                                                   field_normalization_err[1] /
                                                   min(field_normalization_2))

                if NORMALIZE[graph_no]:
                    error_values = \
                        (error_values[0] / field_normalization_err[1],
                         error_values[1] / field_normalization_err[0])
                    group_value = group_value / field_normalization - 1

                groups[opt].append(group_value)
                errors[opt].append(error_values)
        # Now, get these data out in a sorted manner:
        titles = BENCHMARKS
        data_list = []
        errors_list = []
        opt_names = []

        # This specifies where the bars are vertically centered.
        bottom_value = 1 if NORMALIZE[graph_no] else 0

        for opt in sorted(groups):
            if opt != 'no_opts' or not NORMALIZE[graph_no]:
                opt_names.append(OPT_NAMES[opt])
                data_list.append(groups[opt])
                errors_list.append(errors[opt])

        assert len(opt_names) == len(data_list)
        assert len(data_list) == len(errors_list)

        plot = graph.draw_grouped_bar(len(data_list), len(titles), data_list,
                                      titles, errors=errors_list,
                                      labels=opt_names, title=TITLES[graph_no],
                                      bottom=bottom_value,
                                      xlabel="Benchmark",
                                      ylabel=METRIC_NAME[graph_no],
                                      label_rotation=70,
                                      figsize=FIG_SIZE[graph_no],
                                      top_padding=TOP_PADDING[graph_no])

        if FIG_SIZE[graph_no]:
            plot.gcf().set_size_inches(*FIG_SIZE[graph_no])

        graph.save_to(plot, OUTPUT_FILES[graph_no])
        if not args.nohold:
            plot.show()
