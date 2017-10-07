#!/bin/bash

set -e

sudo easy_install virtualenv

mkdir lnt-install
virtualenv lnt-install
source lnt-install/bin/activate

pip install argparse==1.3.0

svn co http://llvm.org/svn/llvm-project/lnt/trunk lnt

lnt-install/bin/python lnt/setup.py develop

lnt create lnt-install/performance_db

deactivate
