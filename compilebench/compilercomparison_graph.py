#!/usr/bin/python

from matplotlib import pyplot
import argparse
import compilebench_graph as plotter
import graph
import json

if __name__ == "__main__":
    parser = \
        argparse.ArgumentParser("A tool for drawing compilation time "
                                "benchmarks between different compilers.")

    parser.add_argument('input_files', nargs='+',
                        help=("Input JSON file as created by the "
                              "benchmarking scripts"))
    parser.add_argument('--nohold', action='store_true', default=False,
                        help="Don't display the figure on the screen.")

    args = parser.parse_args()

    # Load in the data.
    builtup_data = {}
    for input in args.input_files:
        with open(input, 'r') as f:
            data = json.load(f)

            # We build the array from this.
            for benchmark in data:
                if benchmark not in builtup_data:
                    builtup_data[benchmark] = {}

                builtup_data[benchmark][input] = data[benchmark]

    for benchmark in builtup_data:
        # Get the y_data and the error bars out.
        # Also create the legend mappings.
        y_data = []
        x_data = []

        for compiler in builtup_data[benchmark]:
            this_data = []
            measurement = 0
            while str(measurement) + '.sml' in \
                    builtup_data[benchmark][compiler]:
                this_data.append(builtup_data[benchmark][compiler]
                                             [str(measurement) + ".sml"]
                                             ['subprocess_times'])
                x_data.append(measurement)
                measurement += 1

            y_data.append(this_data)

        # Draw the graph.
        plot = \
            graph.draw_multiple_lines(x_data, y_data, None,
                                      plotter.gen_x_label_for(None, benchmark),
                                      y_label='Compile Time (ms)',
                                      title=('Compile time against number of'
                                             ' function declarations'),
                                      legend=None)

        if not args.nohold:
            pyplot.show(True)
