all:
	@echo Downloading and linking to helper libraries...
	git submodule init
	git submodule update
	raco link lib/ragg/ragg/
	raco link lib/whalesong/whalesong/
	@echo Linking and building Pyret locally...
	cd ../..
	raco link -n pyret src/
	raco setup pyret
	@echo Pyret build completed successfully

test:
	@echo Compiling and running tests, no error messages implies all passed
	cd src/tests; \
	raco make compile-tests.rkt parse-tests.rkt; \
	racket parse-tests.rkt; \
	racket compile-tests.rkt;

clean:
	raco setup -c pyret

dangerous-clean:
	find . -name "compiled" | xargs rm -rf

