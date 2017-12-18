#!/bin/bash

set -e

# First, pull any updates:
cd /home/jcw78/mlc
git pull --rebase

# Then rebuild and re-install the compiler:
yes "y" | ./install.sh /home/jcw78/mlc_libraries /home/jcw78/mlc_executable

# And then run the benchmarking script
cd microbench
./benchmark_and_upload.sh "london-x86" /home/jcw78/mlc_executable
