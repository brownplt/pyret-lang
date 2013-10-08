all:
	raco setup -j 2 pyret
	@echo Pyret build completed successfully

dep:
	@echo Downloading and linking to helper libraries...
	git submodule init
	git submodule update lib/ragg lib/whalesong/
	raco link lib/ragg/ragg/
	raco link lib/whalesong/whalesong/
	@echo Linking and building Pyret locally...
	raco link -n pyret src/

pyret-dev: dep
	@echo Downloading private Pyret tests...
	git submodule update examples/pyret-lang-private

test:
	@echo Compiling and running tests, should say 0 errors and 0 failures
	cd src/tests; \
	raco make compile-tests.rkt parse-tests.rkt type-tests.rkt well-formed-tests.rkt indentation-tests.rkt; \
	racket parse-tests.rkt; \
	racket compile-tests.rkt; \
	racket type-tests.rkt; \
	racket well-formed-tests.rkt; \
	racket indentation-tests.rkt

doc:
	@echo Building docs
	cd docs; \
	scribble --htmls lang.scrbl

clean:
	raco setup -c pyret
	rm src/lang/pyret-lib/*.rkt

unlink:
	raco link -r src/
	raco link -r lib/ragg/ragg/
	raco link -r lib/whalesong/whalesong/

dangerous-clean:
	find . -name "compiled" | xargs rm -rf
