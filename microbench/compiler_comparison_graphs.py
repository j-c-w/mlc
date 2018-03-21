import argparse
import graph
import json
import numpy as np

FIELD = ['compile_time', 'execution_time', 'code_size']
NUM_TO_DROP = [3, 3, 3]
FIG_SIZE = [(8, 6), (8, 6), (8, 6)]
OUTPUT_FILE = ['ml_compiler_comparison_compile_time.eps',
               'ml_compiler_comparison_execution_time.eps',
               'ml_compiler_comparison_code_size.eps']
TITLE = ['Comparing ML Compilers: Compile Time',
         'Comparing ML Compilers: Execution Time',
         'Comparing ML Compilers: Executable Size']
Y_AXIS = ['Compile Time (seconds)',
          'Execution Time (seconds)',
          'Executable Size (bytes)']
COMPILERS = sorted(['cmlc-opt-speed', 'cmlc-opt-size', 'cmlc-no-opts',
                    'mlton', 'mosml', 'polyml', 'smlnj'])
PRETTY_COMPILER_NAMES = {
        'cmlc-opt-speed': 'MLC (Optimised for Speed)',
        'cmlc-opt-size': 'MLC (Optimised for Size)',
        'cmlc-no-opts': 'MLC',
        'mlton': 'MLton',
        'mosml': 'MosML',
        'polyml': 'PolyML',
        'smlnj': 'SML/NJ'
    }
TOP_PADDING = [None, None, 100000]
BENCHMARKS = sorted(['alignment', 'dft', 'inlining', 'life', 'mandelbrot',
                     'matrix_multiplication', 'peek', 'tak', 'utils'])


def has_none(data):
    has_none = False

    for datum in data:
        if datum is None:
            has_none = True

    return has_none


if __name__ == "__main__":
    parser = argparse.ArgumentParser(description=("A graphing class for "
                                                  "comparing ML compilers"))

    parser.add_argument("--nohold", dest='nohold', default=False,
                        action='store_true', help='Close graphs after writing')
    parser.add_argument("data_folder", help="Location of benchmarking data")

    for graph_no in range(len(FIELD)):
        args = parser.parse_args()
        # Load the data in for each benchmark.
        data_by_compiler = {}
        for compiler in COMPILERS:
            with open(args.data_folder + "/" + compiler + "-times.json") as f:
                data_by_compiler[compiler] = json.loads(f.read())

        # These are currently grouped by compiler with many lists.  We want
        # to extract this into lists.
        parsed_data_by_compiler = {}
        hasErrors = False
        errors_by_compiler = {}
        benchmarks_names = {}

        for benchmark in BENCHMARKS:
            for compiler in data_by_compiler:
                compiler_data = data_by_compiler[compiler]
                this_data = compiler_data['tests']
                this_benchmark_compiler_data = None

                for data in this_data:
                    if data['name'] == benchmark:
                        this_benchmark_compiler_data = data[FIELD[graph_no]]

                if this_benchmark_compiler_data is None:
                    print "Error reading", benchmark

                data_value = None
                errors = None

                # Get values from the data:
                if hasattr(this_benchmark_compiler_data, '__iter__'):
                    # This can happen if the benchmark failed.
                    if not has_none(this_benchmark_compiler_data):
                        data_value = np.median(this_benchmark_compiler_data)

                        for i in range(NUM_TO_DROP[graph_no]):
                            this_benchmark_compiler_data.remove(
                                    min(this_benchmark_compiler_data))
                            this_benchmark_compiler_data.remove(
                                    max(this_benchmark_compiler_data))

                        errors = (min(this_benchmark_compiler_data),
                                  max(this_benchmark_compiler_data))
                        hasErrors = True
                    else:
                        data_value = None
                        errors = (None, None)
                        hasErrors = True
                else:
                    # If the values are not a list, just take the element.
                    # Obviously no errors can be calculated.
                    if this_benchmark_compiler_data is not None:
                        data_value = this_benchmark_compiler_data
                    else:
                        data_value = 0

                if compiler in parsed_data_by_compiler:
                    parsed_data_by_compiler[compiler].append(data_value)
                    if hasErrors:
                        errors_by_compiler[compiler].append(errors)
                else:
                    parsed_data_by_compiler[compiler] = [data_value]
                    if hasErrors:
                        errors_by_compiler[compiler] = [errors]

        data_list = []
        error_list = [] if hasErrors else None
        compiler_labels = []
        benchmarks = BENCHMARKS

        for compiler in sorted(COMPILERS):
            data_list.append(parsed_data_by_compiler[compiler])
            compiler_labels.append(PRETTY_COMPILER_NAMES[compiler])
            if hasErrors:
                error_list.append(errors_by_compiler[compiler])

        plot = graph.draw_grouped_bar(len(COMPILERS),
                                      len(BENCHMARKS), data_list,
                                      benchmarks, errors=error_list,
                                      labels=compiler_labels,
                                      title=TITLE[graph_no],
                                      xlabel="Benchmark",
                                      ylabel=Y_AXIS[graph_no],
                                      label_rotation=45,
                                      figsize=FIG_SIZE[graph_no],
                                      colors=['#009933', '#333300',
                                              '#ff9900', '#993399',
                                              '#0000ff', '#00ffff',
                                              '#cccc00'],
                                      top_padding=TOP_PADDING[graph_no]
                                      )

        graph.save_to(plot, OUTPUT_FILE[graph_no])
        if not args.nohold:
            plot.show()
