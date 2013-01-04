all:
	git submodule init
	git submodule update
	raco link lib/ragg/ragg/
