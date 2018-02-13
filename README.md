This is an optimizing compiler for ML to Java Bytecode

The ML dialect selected is, roughly speaking:

	val
	fun
	exceptions
	datatypes

# Installation

To install, clone this project:

	git clone https://github.com/j-c-w/mlc
	cd mlc

And run the installation executable

	./install.sh

Pressing (y) as needed. You may need to install some tools (such as
SBT) to build CMLC.

Then, the compiler is executable as:

	cmlc <File Name>

Run produced jar files as:

	java -jar <Jar name>

# Debug Build

To build, install sbt, navigate to this directory and run

	sbt

This will open up the SBT interface.

Execute:

	one-jar

In the SBT prompt. This will produce a Jar file to compiler to JVM assembler.

Open the `debug_install.sh` script. In that script set `jarfile` to
the location of the produced jar file (it will likely be the same).

Now, run:

	./debug_install

This will create a local in-tree installation that does not require
a full-rebuild.
