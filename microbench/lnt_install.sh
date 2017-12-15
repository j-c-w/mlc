#!/bin/bash

set -e
# We cannot set -u as lnt activate fails with it.
# set -u

if [ -d lnt-install ]; then
	read -p "lnt-install exists.  Overwrite?[y/n]" resp

	if [ "$resp" = "y" ]; then
		rm -rf lnt-install
		echo "Continuing"
	else
		echo "Install aborted"
		exit 1
	fi
fi

if [ -d lnt ]; then
	read -p "lnt exists. Overwrite?[y/n]" resp

	if [ "$resp" = "y" ]; then
		rm -rf lnt
		echo "Continuing"
	else
		echo "Install aborted"
		exit 1
	fi
fi

sudo easy_install virtualenv

mkdir lnt-install
virtualenv lnt-install
source lnt-install/bin/activate

pip install argparse==1.3.0

svn co http://llvm.org/svn/llvm-project/lnt/trunk lnt

if [ ! -d lnt ]; then
	echo "Clone of LNT failed!"
	exit 1
fi

lnt-install/bin/python lnt/setup.py develop

lnt create lnt-install/performance_db

deactivate
