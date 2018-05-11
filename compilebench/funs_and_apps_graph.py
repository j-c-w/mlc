import argparse
import graph
import matplotlib.pyplot as plt
import json
import numpy as np


def data_for(data):
    @np.vectorize
    def internal_dat(y, x):
        x = x / 20
        y = y / 20
        avg = []
        count = 0
        for x_v in range(x - 4, x + 4):
            for y_v in range(y - 4, y + 4):
                if str(y_v) + str(x_v) + ".sml" in \
                        data['functions_and_applications']:
                    cell = data['functions_and_applications']\
                        [str(y_v) + str(x_v) + ".sml"]['t_inline.12'][0]
                    avg.append(cell)
                    count += 1

        return np.median(avg)
    return internal_dat


if __name__ == "__main__":
    parser = argparse.ArgumentParser()

    parser.add_argument('data_dir', help='Directory to find data in')

    args = parser.parse_args()

    # TODO --LOAD AND MERGE THE DICTIONARIES.
    with open(args.data_dir + '/cmlc_output_decs_apps_1.json') as f:
        data = json.load(f)

    data_processor = data_for(data)
    X = np.arange(20 * 10, 20 * 70, 20 * 1)
    Y = np.arange(20 * 10, 20 * 70, 20 * 1)

    X, Y = np.meshgrid(X, Y)
    Z = data_processor(X, Y)

    fig = graph.draw_mesh(X, Y, Z)
    axis = fig.gca(projection='3d')
    axis.set_ylabel('Function Declarations')
    axis.set_xlabel('Function Applications')
    axis.set_zlabel('Time spent in Inlining pass (ms)')
    axis.set_title('Time spent in Inlining pass vs number of \n'
                   'declarations and applications')
    line_fig = graph.draw_line(X[0], data_processor(X, X)[0])
    plt.xlabel('Function Declarations and Function Applications')
    plt.ylabel('Time spent in Inlining pass (ms)')
    plt.title('Time spent in Inlining pass with an equal number of\n'
              'declarations and applications')
    # ref_fig = graph.draw_mesh(X, Y, X * Y + 10 * X + 10 * Y)
    fig.show()
    plt.show()
