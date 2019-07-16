.PHONY: all clean build parser web 

all: build parser

build: src/arr/compiler/pyret-parser.js
	pyret --checks none -c src/arr/compiler/pyret.arr -o build/phaseA/pyret.jarr

test: build
	jest --verbose test

runtime-arr: build src/runtime-arr/*.arr
	# Necessary to make relative require paths correct
	cd src/runtime-arr/ && node ../../build/phaseA/pyret.jarr --build-runnable unified.arr

web: build/worker/pyret-grammar.js src/arr/compiler/pyret-parser.js
	mkdir -p build/worker; 
	make build/worker/bundled-node-compile-deps.js
	make build/worker/runtime-files.json
	make build/worker/page.html
	make build/worker/main.js
	pyret --checks none --standalone-file src/webworker/worker-standalone.js --deps-file build/worker/bundled-node-compile-deps.js -c src/arr/compiler/webworker.arr -o build/worker/pyret.jarr

build/worker/runtime-files.json: build/worker/runtime-bundler.js src/runtime/*.arr.j*
	node build/worker/runtime-bundler.js src/runtime/ src/runtime-arr/ build/worker/runtime-files.json

build/worker/runtime-bundler.js: src/webworker/scripts/runtime-bundler.ts
	tsc src/webworker/scripts/runtime-bundler.ts --outFile $@

build/worker/bundled-node-compile-deps.js: src/js/trove/require-node-compile-dependencies.js
	browserify src/js/trove/require-node-compile-dependencies.js -o $@

build/phaseA/pyret-grammar.js: lib/jglr/parser-generator.js 
	mkdir -p build/phaseA 
	mkdir -p build/worker 
	node lib/jglr/parser-generator.js src/js/base/pyret-grammar.bnf build/phaseA/pyret-grammar.js "../../lib/jglr" "jglr/jglr" "pyret-base/js/pyret-parser" 
 
src/arr/compiler/pyret-parser.js: build/phaseA/pyret-grammar.js 
	node build/phaseA/pyret-grammar.js src/arr/compiler/pyret-parser.js 
 
build/worker/pyret-grammar.js: build/phaseA/pyret-grammar.js 
	cp build/phaseA/pyret-grammar.js build/worker/pyret-grammar.js 
 
parser: src/arr/compiler/pyret-parser.js

build/worker/runtime-loader.js: src/webworker/runtime-loader.ts
	tsc $< --outFile $@

build/worker/pyret-api.js: build/worker/pyret-api.ts.js build/worker/runtime-loader.js
	browserify build/worker/pyret-api.ts.js -o $@

build/worker/pyret-api.ts.js: src/webworker/pyret-api.ts
	tsc src/webworker/pyret-api.ts --outFile $@

build/worker/runner.js: build/worker/runner.ts.js
	browserify build/worker/runner.ts.js -o $@

build/worker/runner.ts.js: src/webworker/runner.ts
	tsc src/webworker/runner.ts --outFile $@

build/worker/setup.js: build/worker/setup.ts.js
	browserify build/worker/setup.ts.js -o $@

build/worker/setup.ts.js: src/webworker/setup.ts
	tsc $< --outFile $@

build/worker/main.js: src/webworker/*.ts
	browserify src/webworker/main.ts -p [ tsify ] -o build/worker/main.js

#build/worker/main.ts.js: src/webworker/main.ts
#	tsc $< --outFile $@

build/worker/page.html: src/webworker/page.html
	cp $< $@

clean:
	rm -r -f build/phaseA build/worker
	rm -f src/arr/compiler/pyret-parser.js
	rm -r -f src/arr/runtime-arr/compiled
