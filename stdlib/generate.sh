#!/bin/bash

# This is a shell script that can be used to generate the assembly files
# for all the standard library classes in this folder if it is provided
# with a path that specifies a krakatau installation.

javac *.java
python $1 *.class
rm *.class
