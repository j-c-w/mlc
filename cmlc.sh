#!/bin/bash

# This is a script that redirects inputs to the CMLC compiler.
# It deals with the creation Jar files.

# Do not touch unless you know what you are doing! These should
# be set by the installer!
LIB=AUTOMATICALLY_REPLACED_LIBRARY_LOCATION
JAR=AUTOMATICALLY_REPLACED_JAR_LOCATION

set -e
set -u

# Name of the file without the extension.
base=$(basename ${@:$#})
filename="${base%.*}"

# Build program into a .j file:
java -jar "$JAR"/cmlc.jar "$@"
# Then pass on to the assembler: (Note that the last argument is the file
# name)
krakatau "$filename.j"
# First create the jar file with the local classes
jar cfe "$filename.jar" Main *.class
# Then we package this with the standard libraries:
jar uf "$filename.jar" -C "$LIB" cmlc

# Finally clean up
rm *.class
