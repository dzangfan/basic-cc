update:
	raco pkg remove basic-cc
	raco pkg install

clean:
	raco pkg remove basic-cc
	rm **/compiled -rf
