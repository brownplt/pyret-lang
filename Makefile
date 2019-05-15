.PHONY: all clean build parser web

all: build parser

build:
	pyret -c src/arr/compiler/pyret.arr -o build/phaseA/pyret.jarr

web:
	mkdir -p build/worker; 
	make build/worker/bundled-node-compile-deps.js
	pyret --standalone-file src/webworker/worker-standalone.js --deps-file build/worker/bundled-node-compile-deps.js -c src/arr/compiler/webworker.arr -o build/worker/pyret.jarr

build/worker/bundled-node-compile-deps.js: src/js/trove/require-node-compile-dependencies.js
	browserify src/js/trove/require-node-compile-dependencies.js -o $@

parser:
	mkdir -p build/phaseA
	mkdir -p build/worker
	node lib/jglr/parser-generator.js src/js/base/pyret-grammar.bnf build/phaseA/pyret-grammar.js "../../lib/jglr" "jglr/jglr" "pyret-base/js/pyret-parser"
	node build/phaseA/pyret-grammar.js src/arr/compiler/pyret-parser.js
	cp build/phaseA/pyret-grammar.js build/worker/pyret-grammar.js


clean:
	rm -r -f build/phaseA build/worker
	rm -f src/arr/compiler/pyret-parser.js
