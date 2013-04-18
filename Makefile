all:
	@echo Downloading and linking to helper libraries...
	git submodule init
	git submodule update
	raco link lib/ragg/ragg/
	raco link lib/whalesong/whalesong/
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

clean:
	rm -rf ./lib/pyret-tokenizer/compiled
	rm -rf ./lib/whalesong/whalesong/lang/compiled
	rm -rf ./lib/whalesong/whalesong/lang/unsafe/compiled
	rm -rf ./lib/whalesong/whalesong/lang/js/compiled
	rm -rf ./lib/whalesong/whalesong/lang/check-expect/compiled
	rm -rf ./lib/whalesong/whalesong/lang/private/compiled
	rm -rf ./lib/ragg/ragg/compiled
	rm -rf ./lib/ragg/ragg/cfg-parser/compiled
	rm -rf ./lib/ragg/ragg/codegen/compiled
	rm -rf ./lib/ragg/ragg/codegen/lang/compiled
	rm -rf ./lib/ragg/ragg/lang/compiled
	rm -rf ./lib/ragg/ragg/private/compiled
	rm -rf ./lib/ragg/ragg/rules/compiled
	rm -rf ./src/compiled
	rm -rf ./src/tests/compiled
	rm -rf ./src/lang/compiled
