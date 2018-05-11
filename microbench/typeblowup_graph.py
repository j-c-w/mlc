import argparse
import graph
import json
import numpy as np

COMPILERS = ['cmlc', 'mlton', 'mosml']
PRETTY_NAME_MAP = {
    'cmlc': 'MLC',
    'mlton': 'MLton',
    'mosml': 'MosML'
}
NUM_TO_DROP = 3
COLOR_MAP = {
    'cmlc': '#009933',
    'mlton': '#993399',
    'mosml': '#0000ff'
}

if __name__ == "__main__":
    parser = argparse.ArgumentParser(description=("A plotter for plotting the "
                                                  "compile times of "
                                                  "patalogical cases."))

    parser.add_argument("directory",
                        help="Directory in which to find the data")
    parser.add_argument("--nohold", dest='nohold', default=False,
                        action='store_true',
                        help="Close graphs after they are drawn.")

    args = parser.parse_args()

    # Get the lines:
    data = {}
    for compiler in COMPILERS:
        with open(args.directory + '/' +
                  compiler + "_type_blowup_output.json") as f:
            json_data = json.loads(f.read())['type_blowup']
            
        xmin = 0
        xmax = 0

        yvals = []
        yerrors = []

        # Get all the files that this was computed for out of the
        # Json file.
        while str(xmax) + '.sml' in json_data:
            run_data = json_data[str(xmax) + '.sml']['subprocess_times']

            # Extract the errors and the median.
            for i in range(NUM_TO_DROP):
                run_data.remove(min(run_data))
                run_data.remove(max(run_data))

            point = np.median(run_data)
            yvals.append(point)
            yerrors.append((point - min(run_data), max(run_data) - point))

            xmax += 1

        data[compiler] = {
                'x': range(xmin, xmax),
                'y': yvals,
                'errors': yerrors
        }

    x_values = []
    y_values = []
    y_errors = []
    labels = []
    colors = []

    for compiler in COMPILERS:
        x_values.append(data[compiler]['x'])
        y_values.append(data[compiler]['y'])
        y_errors.append(data[compiler]['errors'])
        colors.append(COLOR_MAP[compiler])
        labels.append(PRETTY_NAME_MAP[compiler])

    fig = graph.draw_multiple_lines(x_values, y_values, colors,
                                    error_bars=y_errors, legend=labels,
                                    title=('Compile Time vs '
                                           'log2(type variables)'),
                                    x_label='log2(type variables)',
                                    y_label='Compile Time (ms)')

    graph.save_to(fig, 'type_blowup_comparison_graph.eps')
    if not args.nohold:
        fig.show(True)
