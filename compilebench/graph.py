#!/usr/bin/python

# This script is for graphing compile data.  It provides
# generic utilities that can be used by all data to do
# graphing.

from matplotlib import cm
from matplotlib import pyplot as plt
from matplotlib import ticker
from mpl_toolkits.mplot3d import Axes3D
import numpy as np
import random


def draw_regression(x_data, y_data, error_bars=None,
                    x_label=None, y_label=None, title=None,
                    legend=None):
    m, c = np.polyfit(x_data, y_data, 1)

    fig = draw_points(x_data, y_data, error_bars=error_bars,
                      x_label=x_label, y_label=y_label, title=title,
                      legend=legend)


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

    handle, = axis.plot(x_data, y_data, color='blue')
    axis.grid(color='black', linestyle='dashed', linewidth=1)
    handles.append(handle)

    # Put error bars on the last one of these.
    if error_bars:
        error_min = [x[0] for x in error_bars]
        error_max = [x[1] for x in error_bars]

        axis.errorbar(x_data, y_data, [error_min, error_max], ecolor='green',
                      elinewidth=1, capsize=1.5)

    if legend:
        axis.legend(legend, loc=2)
    else:
        axis.legend().set_visible(False)

    axis.set_xlim([0, len(x_data)])
    (ymin, ymax) = axis.get_ylim()
    axis.set_ylim([0, ymax])

    return figure


def draw_multiple_lines(x_data, y_data, colors, error_bars=None,
                        x_label=None, y_label=None, title=None, legend=None):
    figure = plt.figure()

    axis = figure.add_subplot(111)
    handles = []

    # If the x_data has a len(y_data) dimension, then there is different
    # x data to plot each time.
    x_length = len(x_data)

    multiple_x = x_length == len(y_data) and hasattr(x_data[0], '__iter__')

    if multiple_x:
        # Then we assume that this was multiple x things to be plotted.
        for index in range(len(x_data)):
            handle = axis.plot(x_data[index], y_data[index], colors[index],
                               linewidth=1)
            handle[0].set_color(colors[index])
            handles += handle
    else:
        for index in range(len(y_data)):
            handle = axis.plot(x_data, y_data[index], colors[index],
                               linewidth=1)
            handle[0].set_color(colors[index])
            handles += handle

    if error_bars:
        if multiple_x:
            for index in range(len(y_data)):
                error_min = [x[0] for x in error_bars[index]]
                error_max = [x[1] for x in error_bars[index]]

                axis.errorbar(x_data[index], y_data[index],
                              [error_min, error_max],
                              elinewidth=1, capsize=2.5, ecolor='black',
                              color=colors[index])
        else:
            for (y_datum, error_bar) in zip(y_data, error_bars):
                error_min = [x[0] for x in error_bar]
                error_max = [x[1] for x in error_bar]

                axis.errorbar(x_data, y_datum, [error_min, error_max],
                              elinewidth=1, capsize=2.5, ecolor='black')

    if x_label:
        axis.set_xlabel(x_label)

    if y_label:
        axis.set_ylabel(y_label)

    if title:
        axis.set_title(title)

    if legend:
        axis.legend(legend, loc=2)

    axis.grid(color='black', linestyle='dashed', linewidth=1)

    for index in range(len(colors)):
        plt.gca().get_lines()[index].set_color(colors[index])

    (ymin, ymax) = axis.get_ylim()
    (xmin, xmax) = axis.get_xlim()

    axis.set_ylim(0, ymax)
    axis.set_xlim(0, xmax)

    return figure


def draw_mesh(X, Y, Z):
    # Plot the surface.
    figure = plt.figure()
    axis = figure.gca(projection='3d')

    axis.plot_surface(X, Y, Z, cmap=cm.coolwarm,
                      linewidth=0, cstride=2, rstride=2,
                      antialiased=False)

    return figure


def draw_stacked_line(number_of_points, x_data, y_data, error_bars=None,
                      colors=None, x_label=None, y_label=None, title=None,
                      legend=None, ylim_max=None):
    """ This takes y_data, error_bars as a list of tuples.  Each tuple
        element corresponds to a stacked line in the graph.  Colors
        are passed as a list and are generated if not specified.

        We assume that x_data is already cumulative.

        It returns a figure that can be prettified.  """
    if not colors:
        # Use a random sequence for colors with a constant seed for
        # reproduceability.
        random_seq = random.Random(18)
        colors = []

        for i in range(number_of_points):
            if legend:
                random_seq = random.Random(2 + hash(legend[i]))
            color_value = random_seq.randint(0x100000, 0xFFFFFF)
            hex_color = "#" + hex(color_value)[2:]
            colors.append(hex_color)

    figure = plt.figure()

    axis = figure.add_subplot(111)
    handles = []
    axis.grid(color='black', linestyle='dashed', linewidth=1)

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

        axis.errorbar(x_data, y_idata, [error_min, error_max], color='black',
                      elinewidth=1, capsize=1)

    axis.set_xlim([0, len(x_data) - 1])
    (y_min, y_max) = axis.get_ylim()
    axis.set_ylim([0, y_max + 100])

    # If an explicit max was passed, then use that.
    if ylim_max:
        axis.set_ylim([0, ylim_max])

    if legend:
        axis.legend(legend, loc=2)

    return figure


def draw_stacked_bar(no_stacks, no_xgroups, stacked_data, x_names,
                     errors=None, colors=None, labels=None, y_label=None,
                     x_label=None, title=None):
    fig, ax = plt.subplots()
    width = 0.66
    x_values = range(no_xgroups)
    handles = []

    if colors is None:
        random_seq = random.Random(0)
        colors = []

        for i in range(no_stacks):
            if labels:
                random_seq = random.Random(-1 + hash(labels[i]))

            color_value = random_seq.randint(0x100000, 0xFFFFFF)
            hex_color = "#" + hex(color_value)[2:]
            colors.append(hex_color)

    bottoms = [0.0] * no_xgroups
    for i in range(no_stacks):
        handle = ax.bar(x_values, stacked_data[i], width, color=colors[i],
                        bottom=bottoms)
        for j in range(len(bottoms)):
            bottoms[j] += stacked_data[i][j]

        handles.append(handle)

    if title:
        ax.set_title(title)

    if y_label:
        ax.set_ylabel(y_label)

    if x_label:
        ax.set_xlabel(x_label)

    ax.set_xticks(x_values)
    ax.set_xticklabels(x_names, rotation=70)

    ax.yaxis.grid(True, linestyle='dotted')
    if labels:
        fig.legend(labels, loc='upper right', bbox_to_anchor=(0.9, 0.88))

    fig.tight_layout()
    return fig


def draw_grouped_bar(no_bargroups, no_xgroups, group_data, group_names,
                     errors=None, colors=None, labels=None,
                     xlabel=None, ylabel=None, title=None, bottom=0,
                     logarithmic=False, label_rotation=0, approx_ticks=20,
                     figsize=None, random_color_seed=-1, top_padding=None):
    """ Draw a bar chart in groups.  'groups' is the number of groups. """
    # create plot
    fig, ax = plt.subplots(figsize=figsize)
    index = np.arange(no_xgroups)
    bar_width = 0.7 / no_bargroups
    bar_spacing = 0.3
    opacity = 1.0

    if colors is None:
        random_seq = random.Random(random_color_seed)
        colors = []

        for i in range(no_bargroups):
            if labels:
                if labels[i] == 'dse':
                    random_seq = random.Random(random_color_seed + hash('dce'))
                else:
                    random_seq = \
                            random.Random(random_color_seed + hash(labels[i]))

            color_value = random_seq.randint(0x100000, 0xFFFFFF)
            hex_color = "#" + hex(color_value)[2:]
            colors.append(hex_color)

    if not labels:
        labels = [''] * no_bargroups

    # This is a list that stores the locations of any data items
    # that were entered as None.  These are assumed to have failed,
    # and a marker is left to indicate this.
    failed_data = []

    for group_no in range(no_bargroups):
        data = group_data[group_no]
        y_err = np.array(errors[group_no]) if errors else None

        if y_err is not None:
            for i in range(len(y_err)):
                if y_err[i][0] is not None and y_err[i][1] is not None:
                    y_err[i] = (data[i] + bottom - y_err[i][0],
                                y_err[i][1] - data[i] - bottom)
                else:
                    # Errors are none, so make them actually 0.
                    y_err[i] = (0, 0)

            # Rotate the errors so the are in the correct format.
            y_err = y_err.T

        error_config = {'elinewidth': 1}

        for item in range(len(data)):
            if data[item] is None:
                # Mark this index as None.  We will mark it as an error later.
                failed_data.append((group_no, item))
                # The plotter cannot handle None, so insert it as 0.
                data[item] = 0

        plt.bar(index + bar_width * group_no, data, bar_width,
                alpha=opacity, color=colors[group_no], label=labels[group_no],
                yerr=y_err, bottom=bottom, capsize=2.5, error_kw=error_config)

    # Add the labels:
    # Set the position of the x ticks
    ax.set_xticks([pos + no_xgroups / 2 * bar_width
                   for pos in range(no_xgroups)])

    # Set the labels for the x ticks
    ax.set_xticklabels(group_names, rotation=label_rotation)

    (ymin, ymax) = plt.ylim()
    # Make the ylim higher if it is specified.
    if top_padding:
        ax.set_ylim(ymin, ymax + top_padding)

    (ymin, ymax) = plt.ylim()
    # Draw the failure marks for benchmarks that failed.
    for (group_number, bargroup_number) in failed_data:
        yrange = ymax - ymin
        ax.annotate(labels[group_number] + " Failed", rotation=90,
                    xy=(0, 0), fontsize=str(65 * bar_width),
                    va='bottom', ha='left',
                    xytext=(bargroup_number + bar_width * group_number -
                            bar_width / 2, ymin + yrange / 60))

    # Draw gridlines between each bar and box colors below each to
    # make it clear which optimizations are being applied with 0 bar size.
    for pos in range(0, no_xgroups):
        if pos != 0:
            ax.annotate("", xy=(pos - bar_spacing / 1.5, ymin),
                        xytext=(pos - bar_spacing / 1.5, ymax),
                        arrowprops=dict(arrowstyle="-", linestyle="dotted",
                                        connectionstyle="arc3, rad=0"))

        if bottom != 0:
            # Only draw the color references if the bottom is not at 0.
            # Make the ticks invisible in this case.
            ax.tick_params(axis='x', which='both', length=0)
            for bar in range(no_bargroups):
                ax.add_patch(
                    plt.Rectangle((-bar_width / 2 + bar * bar_width + pos,
                                   ymin), bar_width,
                                  -(ymax - ymin) / 70, facecolor=colors[bar],
                                  clip_on=False, linewidth=0))

    if xlabel:
        plt.xlabel(xlabel)
    if ylabel:
        plt.ylabel(ylabel)
    if title:
        plt.title(title)

    plt.legend(ncol=2)
    plt.tight_layout()

    # Set the x-axis so the gridlines are symetric.
    ax.set_xlim(-0.21, no_xgroups - 0.21)

    if logarithmic:
        # If the ymin is less than the bottom, then we need
        # to use two logarithmic scales.
        loc = ticker.LogLocator(numticks=20, presets=bottom)
        ax.yaxis.set_major_locator(loc)
    else:
        # We can use a single linear scale, but the numbers must
        # be engineered so it passes through the 'bottom'.
        loc = ticker.MaxNLocator(nbins=approx_ticks, min_n_ticks=approx_ticks,
                                 integer=True)
        ax.yaxis.set_major_locator(loc)

    # Add the x grid.
    ax.xaxis.grid(False)
    ax.grid(which='major', axis='y', linestyle='dotted')

    return plt


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
