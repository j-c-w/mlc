import os

# This is a script for parsing perf records.  It is intended to be used in
# conjunction with other scripts to draw graphs.


def parse_perf_string(perf_lines):
    """ This takes a perf file split into lines.  It returns a
        dictionary containing all the stats recorded.
    """

    res_dict = {}

    for line in perf_lines:
        trimmed_line = line.strip(' ')
        if trimmed_line[0].isdigit():
            # Record this line:
            peices = trimmed_line.split(' ')

            # The first element is the recorded number.  The second
            # non-space element is the name of that thing.
            # There may be tailing comments, but those can be worked out
            # (e.g. percentages)
            name_index = 1
            while not peices[name_index]:
                name_index += 1

            # Then peices[0] is the number (which may be real or int)
            try:
                value = float(int(peices[0].replace(',', '')))
            except ValueError:
                # Try making it a float:
                try:
                    value = float(peices[0].replace(',', ''))
                except ValueError:
                    raise ValueError("Could not convert the string " +
                                     peices[0] + " to a number.  Is this a "
                                     "perf file?")

            # Get the name
            name = peices[name_index]
            res_dict[name] = value

    return res_dict


def parse_perf(perf_file):
    """ This takes a perf file.  It returns a dictionary containing
        all the stats recorded.  Recorded stats are indexed by stat name.
    """

    with open(perf_file) as f:
        return parse_perf_string(f.readlines())


def parse_perf_files(prefix):
    """ Given some prefix, this tries to parse perf files
        <prefix>0 ... <prefix>N

        It keeps going until <prefix>N does not exist.

        This collects dictionaries from each file and merges them.
    """

    index = 0
    files = []
    while os.path.exists(prefix + str(index) + ".perf"):
        files.append(prefix + str(index) + ".perf")
        index += 1

    if index == 0:
        # This is likely an error, as it means no files were found.
        raise ValueError("Files starting with " + prefix + " not found. "
                         "Looked for " + prefix + str(0) + ".perf")
    else:
        return parse_perf_files_list(files)


def parse_perf_files_list(files):
    dictionaries = []
    for file in files:
        # The file exists, so get the dictionary for it:
        dictionaries.append(parse_perf(file))

    # Finally, go through and concatenate these into a single dict.
    result_dict = {}

    for dict in dictionaries:
        for item in dict:
            # Append the item to the result_dict items if it is already there.
            if item in result_dict:
                result_dict[item].append(dict[item])
            else:
                result_dict[item] = [dict[item]]

    return result_dict
