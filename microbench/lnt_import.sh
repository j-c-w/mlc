#!/bin/bash

set -e

source lnt-install/bin/activate

# Import the reports
lnt import lnt-install/performance_db reports/current/*.json

# Then move the reports to a backup folder.
date=$(date +"%Y_%m_%d_%H_%M")

mkdir -p reports/prev-$date/
mv reports/current/*.json  reports/prev-$date/
mv reports/current/*.perf  reports/prev-$date/

deactivate
