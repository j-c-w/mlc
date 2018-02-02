This is a sub-package of CMLC for drawing graphs of compiler
performance.

The structure is as follows:
	- benchmarks
		Contains the benchmarks.  The benchmark folders are the name
		of each benchmark.

		Each benchmark consists of the file:

			gen
		
		This is a script that when run without arguments produces
		the files:

			main0.sml
			main1.sml
			...
			mainN.sml

		For any value of N deemed appropriate.

		In addition, there is a .config file, which specifies
		information about the graph type.  The format is:

			Title="TITLE"
			SubTitle="SUB TITLE"
			etc.

#Script Behaviour

The script runs the gen script.  It finds all the generated main files,
and builds those one by one measuring the time.  
