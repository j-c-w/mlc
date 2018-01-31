#!/bin/bash

# This is a script that redirects inputs to the CMLC compiler.
# It deals with the creation Jar files.

# Do not touch unless you know what you are doing! These should
# be set by the installer!
LIB=AUTOMATICALLY_REPLACED_LIBRARY_LOCATION
JAR=AUTOMATICALLY_REPLACED_JAR_LOCATION
ASSEMBLER=AUTOMATICALLY_REPLACED_ASSEMBLER_LOCATION
TEMP_DIR=$(mktemp -d)

set -e
set -u

base=${@:$#}
# Name of the file without the extension.
filename="${base%.*}"

# Build program into a .j file:
# We request a bigger stack for the recursion.
java -Xss32m -jar "$JAR" "$@"
# Then pass on to the assembler: (Note that the last argument is the file
# name).  Output of Krakatau thrown away becase we don't want the screen filled
# with millions of classes.
$ASSEMBLER "$filename.j" -out "$TEMP_DIR" > /dev/null || (echo "Assemble failed.  Please report this as a bug. "; exit 1)
# Delete the assembly file:
rm "$filename.j"
# First create the jar file with the local classes
jar cfe "$filename.jar" Main -C "$TEMP_DIR" .
# Then we package this with the standard libraries:
jar uf "$filename.jar" -C "$LIB" cmlc

# Finally clean up
rm $TEMP_DIR/*.class
