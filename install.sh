#!/bin/bash

# Assembler location
KRAKATAU_DIRECTORY=/usr/local/lib/krakatau
KRAKATAU_LINK="https://github.com/Storyyeller/Krakatau"

set -e
set -u

if [ $# -ge 1 ]; then
	LIBRARY_TARGET="$1"
else
	LIBRARY_TARGET=/usr/local/lib/cmlc
fi

if [ $# -ge 2 ]; then
	COMPILER_TARGET="$2"
else
	COMPILER_TARGET=/usr/local/bin/cmlc
fi

echo "Library target is $LIBRARY_TARGET"
echo "Compiler target is $COMPILER_TARGET"

read -r -p "OK? [y/N] " response
response=${response,,}    # tolower
if [[ ! "$response" =~ ^(yes|y)$ ]]; then
	echo "Aborted"
	exit 1
fi

echo "OK! Installing!"

help() {
	cat <<!
This is an installation script for the compiler.  It installs the standard
libraries into /usr/local/lib/cmlc/cmlc and the compiler into /usr/local/bin
If krakatau is not installed, that is also installed into /usr/local/bin
since it is needed for compilation.
 
Usage is:
./install.sh [LIBRARY_FOLDER] [COMPILER_TARGET]

Default for library folder is /usr/local/lib/cmlc
Default for compiler target is /usr/local/bin/cmlc

The installation settings are configurable from within the script.

Run within the directory it is in.  It will build the compiler.
!
}

install_libraries() {
	echo "Making the installation directory..."
	mkdir -p $LIBRARY_TARGET/lib

	echo "Building libraries from source..."
	if [ ! -d "./stdlib" ]; then
		echo "Error: Expecting to find standard libraries in ./stdlib"
		echo "Has this been executed in the directory it was found in?"
		exit 1
	fi

	echo "Found sources. Compiling... (This may take a while)"
	javac stdlib/**/*.java
	echo "Compile complete. Installing into $LIBRARY_TARGET/lib"
	cd stdlib
	cp -r ./**/*.class --parents $LIBRARY_TARGET/lib
	cd ..
	echo "Installation of libraries complete!"
}

install_compiler() {
	compiler_folder=$LIBRARY_TARGET/compiler
	echo "Installing the compiler to $COMPILER_TARGET"
	echo "Cleaning the build space..."
	sbt clean > /dev/null
	echo "Building... (This may take several minutes)"

	sbt_output="$(sbt one-jar)"

	if [ $(grep -e "^[error]" <<< "$sbt_output" | wc -l)  -ne 0 ]; then
        # There was a compile error
		# Hi! for those interested in helping, to see the compile
		# error, run 'sbt' then when sbt starts, type 'compile'<CR>
		# Happy debugging!
        echo "Compile error. Please submit this as a bug report."
        exit 255
    fi

    jarfile=$(grep -e "Packaging" <<< "$sbt_output" | grep "one-jar" | cut -d' ' -f 3)

	echo "Build OK!"
	echo "Jar file for compiler is $jarfile"

	if [ ! -f $jarfile ]; then
		# If you got here and want to fix this, replace
		# 'jarfile=...' above with a path to the jarfile
		# that gets created when executing 'sbt one-jar'.
		echo "Whoops! Couldn't find $jarfile..."
		echo "This is a bug.  Please report this"
		exit 1
	fi

	echo "Moving compiler Jar file into $compiler_folder..."

	mkdir -p $compiler_folder

	cp $jarfile $compiler_folder/cmlc.jar

	echo "Moving compiler shell script into $compiler_folder..."
	cp cmlc.sh $compiler_folder/cmlc

	echo "Setting permissions..."
	chmod +x cmlc.sh

	echo "Initializing paths to standard libraries in shell script..."
	sed -i "s!AUTOMATICALLY_REPLACED_LIBRARY_LOCATION!$LIBRARY_TARGET/lib!g" $compiler_folder/cmlc
	echo "Initializing paths to the compiler jar in shell script..."
	sed -i "s!AUTOMATICALLY_REPLACED_JAR_LOCATION!$LIBRARY_TARGET/compiler!g" $compiler_folder/cmlc

	echo "Creating symlink to $COMPILER_TARGET..."
	ln -sf $compiler_folder/cmlc $COMPILER_TARGET
	echo "Setting permissions to $COMPILER_TARGET..."
	chmod +x "$COMPILER_TARGET"
	echo "Done!"
}

if [ $# -ne 0 ]; then
	help
fi

echo "Checking for sbt..."
if ! type "sbt" > /dev/null; then
	echo "sbt not installed.  Please install sbt before continuing."
	echo "http://www.scala-sbt.org/"
	exit 1
fi

echo "Found."

echo "Checking for javac..."

if ! type "javac" > /dev/null; then
	echo "javac not installed.  Please install the JDK before continuing."
	echo "https://www.java.com/en/download/help/linux_install.xml"
	exit 1
fi

echo "Found."


# Make the library directory:
echo "Checking for krakatau installation...."
if ! type "krakatau" > /dev/null; then
	echo "krakatau not found. Installing from: $KRAKATAU_LINK"

	if [ -d $KRAKATAU_DIRECTORY ]; then
		echo "Krakatau installation not found, but installation target "
		echo "$KRAKATAU_DIRECTORY exists.  Perhaps chose a new installation"
		echo "directory?"
		exit 1
	fi

	echo "Cloning..."
	git clone --depth=1 $KRAKATAU_LINK $KRAKATAU_DIRECTORY > /dev/null
	echo "Clone finished."
	echo "Creating symlinks..."

	ln -sf /usr/local/bin/krakatau "$KRAKATAU_DIRECTORY"/krakatau
fi

echo "Checking for existing library installation...."

if [ -d $LIBRARY_TARGET ]; then
	read -r -p "$LIBRARY_TARGET seems to exist. Continue? [y/N] " response
	response=${response,,}    # tolower
	if [[ "$response" =~ ^(yes|y)$ ]]; then
		install_libraries
	else
		echo "Aborted"
	fi
else
	install_libraries
fi

echo "Checking for existing compiler installation..."
if [ -f $COMPILER_TARGET ]; then
	echo "$COMPILER_TARGET seems to exist. "
	echo "You can either replace it, or re-run this script setting a different"
	echo "executable name."
	read -r -p "Replace? [y/N] " response
	response=${response,,}    # tolower
	if [[ "$response" =~ ^(yes|y)$ ]]; then
		install_compiler
	else
		echo "Aborted"
	fi
else
	install_compiler
fi
