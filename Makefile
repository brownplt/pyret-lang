all:
	@echo Downloading and linking to helper libraries...
	git submodule init
	git submodule update
	raco link lib/ragg/ragg/
	@echo Linking Pyret locally...
	cd ../..
	raco link -n pyret src/
	@echo Pyret build completed successfully

test:
	@echo Compiling and running tests, no error messages implies all passed
	cd src/tests; \
	raco make compile-tests.rkt parse-tests.rkt; \
	racket parse-tests.rkt; \
	racket compile-tests.rkt;
