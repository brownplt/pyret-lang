all:
	raco setup pyret
	@echo Pyret build completed successfully

dep:
	@echo Downloading and linking to helper libraries...
	git submodule init
	git submodule update
	raco link lib/ragg/ragg/
	raco link lib/whalesong/whalesong/
	@echo Linking and building Pyret locally...
	cd ../..
	raco link -n pyret src/

test:
	@echo Compiling and running tests, should say 0 errors and 0 failures
	cd src/tests; \
	raco make compile-tests.rkt parse-tests.rkt; \
	racket parse-tests.rkt; \
	racket compile-tests.rkt;

clean:
	raco setup -c pyret

unlink:
	raco link -r src/
	raco link -r lib/ragg/ragg/
	raco link -r lib/whalesong/whalesong/

dangerous-clean:
	find . -name "compiled" | xargs rm -rf

