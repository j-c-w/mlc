This is an optimizing compiler for ML.

The ML dialect selected is, roughly speaking:

	val
	fun

Some notes where this dialect differs are: ';' can not just
be thrown anywhere. They are only for expressions with
other subsequent expressions.

# Building

To build, install sbt, navigate to this directory and run

	sbt one-jar

This will create a standalone jar, executed as:

	java -jar ...(jar name)... [Flags] filename
