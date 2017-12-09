#!/bin/bash

# This script installs the compiler executable and standard libraries
# WITHOUT REBUILD!
# It is intended for development use so that we do not have to clean
# everything up every time.

TARGET=$(pwd)/debug_install
jarfile="$(pwd)/target/scala-2.11/cmlc_2.11-0.0.1-one-jar.jar"
compiler_folder=$TARGET/compiler
KRAKATAU_DIRECTORY=$TARGET/krakatau
KRAKATAU_EXECUTABLE=$KRAKATAU_DIRECTORY/assemble.py
KRAKATAU_LINK="https://github.com/Storyyeller/Krakatau"

mkdir -p $TARGET/lib
# Build and copy the libraries (build takes little time if it's done, so
# do it anyway)
javac stdlib/**/*.java
cd stdlib
cp -r ./**/*.class --parents $TARGET/lib
cd ..
echo "Installation of libraries complete!"

# Copy the JAR file into the executable location:

mkdir -p $compiler_folder

cp $jarfile $compiler_folder/cmlc.jar

echo "Moving compiler shell script into $TARGET/compiler..."
cp cmlc.sh $TARGET/compiler/cmlc

echo "Setting permissions..."
chmod +x $TARGET/compiler/cmlc

echo "Installing krakatau from $KRAKATAU_LINK..."

if [ -f $KRAKATAU_EXECUTABLE ]; then
	echo "Krakatau installation already found. "
else
	mkdir -p $KRAKATAU_DIRECTORY

	echo "Cloning..."
	git clone --depth=1 $KRAKATAU_LINK $KRAKATAU_DIRECTORY > /dev/null
	echo "Clone finished..."
	echo "Checking that executable exists..."

	if [ ! -f "$KRAKATAU_EXECUTABLE" ]; then
		# Hi! If you found this and are interested in debugging, take a look
		# at the krakatau project.  There should be an executbale in there
		# somewhere, that may have moved...  This part of the install script
		# needs to point to that executable.
		echo "Error: Expected an executable at "
		echo "$KRAKATAU_EXECUTABLE" echo "This is a bug, please report it."
		exit 1
	fi

	echo "Setting permissions on assembler..."
	chmod +x $KRAKATAU_EXECUTABLE
fi

echo "Initializing paths to standard libraries in shell script..."
sed -i "s!AUTOMATICALLY_REPLACED_LIBRARY_LOCATION!$TARGET/lib!g" $compiler_folder/cmlc
echo "Initializing paths to the compiler jar in shell script..."
sed -i "s!AUTOMATICALLY_REPLACED_JAR_LOCATION!$TARGET/compiler!g" $compiler_folder/cmlc
echo "Initializing paths to the assembler in shell script..."
sed -i "s!AUTOMATICALLY_REPLACED_ASSEMBLER_LOCATION!$KRAKATAU_EXECUTABLE!g" $compiler_folder/cmlc

echo "Creating symlink to cmlc..."
ln -sf $compiler_folder/cmlc cmlc
echo "Setting permissions to cmlc..."
chmod +x "cmlc"
echo "Done!"
