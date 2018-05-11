import argparse
import graph
import json

FIELD = 'code_size'
OPTIMIZATIONS = sorted(['copyprop', 'dce', 'inline', 'no_opts', 'peephole',
                        'simplify', 'tce'])
BENCHMARKS = sorted(['alignment', 'dft', 'inlining', 'knuth_bendix', 'life',
                     'mandelbrot', 'matrix_multiplication', 'tak', 'utils'])

if __name__ == "__main__":
    parser = argparse.ArgumentParser(description=("A script for drawing code "
                                                  "size comparisons between "
                                                  "passes."))
    parser.add_argument('data_folder', help='Location of the json files')
    parser.add_argument('--nohold', dest='nohold', default=False,
                        action='store_true')
    args = parser.parse_args()

    opt_data = {}
    # Get the JSON files:
    for opt in OPTIMIZATIONS:
        with open(args.data_folder + "/cmlc-" + opt + ".json") as f:
            opt_data[opt] = json.loads(f.read())

    # Extract the normalizing points.
    no_opt_data = {}

    for opt in opt_data['no_opts']['tests']:
        no_opt_data[opt['name']] = opt[FIELD]

    # Now, extract the data and normalize it.
    data_points = []
    used_opts = []

    for opt in sorted(OPTIMIZATIONS):
        if opt != 'no_opts':
            used_opts.append(opt)
            # Put the data in a dictionary, then sort the keys and
            # extract them sorted.
            data_dict = {}
            for test in opt_data[opt]['tests']:
                if test['name'] in BENCHMARKS:
                    data_dict[test['name']] = \
                        float(test[FIELD]) / float(no_opt_data[test['name']]) \
                        - 1

            this_list = []
            for benchmark in sorted(data_dict):
                this_list.append(data_dict[benchmark])

            data_points.append(this_list)

    fig = graph.draw_grouped_bar(len(used_opts),
                                 len(BENCHMARKS), data_points,
                                 sorted(BENCHMARKS),
                                 labels=sorted(used_opts),
                                 bottom=1, label_rotation=70,
                                 figsize=(8, 6),
                                 xlabel="Benchmarks",
                                 ylabel="Code Size (Bytes)",
                                 title=("Code Size under Individual "
                                        "Optimizations"))

    graph.save_to(fig, 'individual_passes_code_size.eps')

    if not args.nohold:
        fig.show()
