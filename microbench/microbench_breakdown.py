import argparse
import graph
import json
import numpy
import os

CATEGORIES_LIST = ['Verification', 'Lower', 'Optimisations', 'Assembler',
                   'Linker']

CATEGORIES = {
    'Optimisations': ['tail_elim', 't_inline', 'pre_lower_simplify',
                      'copy_prop', 'simplify', 'byte_dce', 'peephole'],
    'Verification': ['lex', 'ast', 'ast_change_names', 'typecheck'],
    'Lower': ['loweer_ast', 't_outline', 'lambda_lift', 'lower_program',
              'lower_variables', 'lower_tir', 'output'],
    'Assembler': ['assemble'],
    'Linker': ['link']
}


def get_data_from(passes):
    results = {}

    for cat in CATEGORIES:
        data = []
        for pass_name in CATEGORIES[cat]:
            for run_pass_name in passes:
                if run_pass_name.startswith(pass_name):
                    data.append(passes[run_pass_name])

        sum = [0.0] * len(data[0])

        for datum in data:
            for i in range(len(sum)):
                sum[i] += datum[i]

        results[cat] = numpy.median(sum)

    return results


if __name__ == "__main__":
    parser = argparse.ArgumentParser(description="A Script for drawing the "
                                                 "breakdown of passes in MLC "
                                                 "while building "
                                                 "microbenchmarks")

    parser.add_argument('--nohold', dest='nohold', default=False,
                        action='store_true', help='close graphs once drawn')
    parser.add_argument('data_folder', help='folder where data is lcoated')

    args = parser.parse_args()

    with open(args.data_folder + '/microbench_build_cmlc.json') as f:
        data = json.loads(f.read())

    benchmarks = []
    processed_data = {}

    for benchmark_file in data:
        # We need to get the benchmark name out:
        benchmark_name = \
            os.path.basename(benchmark_file)

        # Now get the data for the runs:
        stacked_data = get_data_from(data[benchmark_file])

        processed_data[benchmark_name] = stacked_data
        benchmarks.append(benchmark_name)

    # This is stored in a category-major format.
    stacked_data_list = []

    for category in CATEGORIES_LIST:
        this_data = []
        for benchmark in sorted(benchmarks):
            this_data.append(processed_data[benchmark][category])
        stacked_data_list.append(this_data)

    print stacked_data_list

    fig = graph.draw_stacked_bar(len(CATEGORIES_LIST), len(benchmarks),
                                 stacked_data_list, sorted(benchmarks),
                                 labels=CATEGORIES_LIST, x_label='Benchmark',
                                 y_label='Compile Time (ms)',
                                 title=('Compile Time Breakdown in MLC'))
    graph.save_to(fig, 'compile_time_breakdown.eps')
    if not args.nohold:
        fig.show()
