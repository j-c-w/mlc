#!/usr/bin/python

# This script is for graphing compile data.  It provides
# generic utilities that can be used by all data to do
# graphing.

from matplotlib import pyplot as plt
import numpy as np
import random


def draw_line(x_data, y_data, error_bars=None,
              x_label=None, y_label=None, title=None,
              legend=None):
    figure = plt.figure()

    axis = figure.add_subplot(111)
    handles = []

    if x_label:
        axis.set_xlabel(x_label)

    if y_label:
        axis.set_ylabel(y_label)

    if title:
        axis.set_title(title)

    handle, = axis.plot(x_data, y_data)
    handles.append(handle)

    # Put error bars on the last one of these.
    if error_bars:
        error_min = [x[0] for x in error_bars]
        error_max = [x[1] for x in error_bars]

        axis.errorbar(x_data, y_data, [error_min, error_max], color='green')

    if legend:
        axis.legend(legend, loc=2)

    return figure


def draw_multiple_lines(x_data, y_data, error_bars=None,
                        x_label=None, y_label=None, title=None, legend=None):
    figure = plt.figure()

    axis = figure.add_subplot(111)
    handles = []

    for data in y_data:
        handle = axis.plot(x_data, data)
        handles += handle

    if error_bars:
        for (datum, error_bar) in zip(y_data, error_bars):
            error_min = [x[0] for x in error_bar]
            error_max = [x[1] for x in error_bar]

            axis.errorbar(x_data, datum, [error_min, error_max])

    if x_label:
        axis.set_xlabel(x_label)

    if y_label:
        axis.set_ylabel(y_label)

    if title:
        axis.set_title(title)

    if legend:
        axis.legend(legend, loc=2)

    return figure


def draw_stacked_line(number_of_points, x_data, y_data, error_bars=None,
                      colors=None, x_label=None, y_label=None, title=None,
                      legend=None):
    """ This takes y_data, error_bars as a list of tuples.  Each tuple
        element corresponds to a stacked line in the graph.  Colors
        are passed as a list and are generated if not specified.

        We assume that x_data is already cumulative.

        It returns a figure that can be prettified.  """
    if not colors:
        # Use a random sequence for colors with a constant seed for
        # reproduceability.
        random_seq = random.Random(5)
        colors = []

        for i in range(number_of_points):
            color_value = random_seq.randint(0x100000, 0xFFFFFF)
            hex_color = "#" + hex(color_value)[2:]
            colors.append(hex_color)

    figure = plt.figure()

    axis = figure.add_subplot(111)
    handles = []

    if x_label:
        axis.set_xlabel(x_label)

    if y_label:
        axis.set_ylabel(y_label)

    if title:
        axis.set_title(title)
    last_top = 0
    for i in range(number_of_points):
        y_idata = [data[i] for data in y_data]
        handle = axis.fill_between(x_data, last_top, y_idata,
                                   facecolor=colors[i], alpha=0.7)
        handles.append(handle)
        last_top = y_idata

    # Put error bars on the last one of these.
    if error_bars:
        error_min = [x[0] for x in error_bars]
        error_max = [x[1] for x in error_bars]

        axis.errorbar(x_data, y_idata, [error_min, error_max], color='blue')

    if legend:
        axis.legend(legend, loc=2)

    return figure


def generate_min_max_median(data, narrowing_function=None,
                            averaging_function=None,
                            delete_min_max=None):
    temp_list = data[:]

    # Delete the min and the max if requested.
    if delete_min_max:
        for i in range(0, delete_min_max):
            temp_list.remove(min(temp_list))
            temp_list.remove(max(temp_list))

    # Get the original item out of data.
    selected_medians = median(temp_list, narrowing_function)

    # Average the medians:
    if averaging_function:
        averaged_median = averaging_function(selected_medians)
    else:
        averaged_median = sum(selected_medians) / len(selected_medians)

    if narrowing_function:
        top_vals = [narrowing_function(x) for x in temp_list]
    else:
        top_vals = temp_list

    if narrowing_function:
        averaged_median_val = narrowing_function(averaged_median)
    else:
        averaged_median_val = averaged_median

    # Return the error bars and the new data.
    return (averaged_median_val - min(top_vals),
            max(top_vals) - averaged_median_val, averaged_median)


# This returns the list of elements that are medians of
# the passed list.
def median(elems, narrowing_function=None):
    if narrowing_function:
        # Calculate the median of the narrowed list:
        sub_medians = median(map(narrowing_function, elems))

        medians = []
        for i in range(len(elems)):
            if narrowing_function(elems[i]) in sub_medians:
                medians.append(elems[i])

        return set(medians)
    else:
        n = len(elems)
        if n < 1:
            return None
        elif n % 1 == 2:
            return set(sorted(elems)[n // 2])
        else:
            return set(sorted(elems)[n // 2 - 1: n // 2 + 1])


def save_to(plot, filename):
    plot.savefig(filename, format='eps', dpi=1000)


if __name__ == "__main__":
    # Run a few tests.
    highlights = []
    for i in range(100):
        highlights.append(i)
    fig = draw_stacked_line(100, [0, 1],
                            [tuple(highlights), tuple(highlights)], None, None)
    fig.show()
    plt.show()
