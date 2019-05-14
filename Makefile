.PHONY: all clean build parser

all: build parser

build:
	pyret -c src/arr/compiler/pyret.arr -o build/phaseA/pyret.jarr

parser:
	mkdir -p build/phaseA
	node lib/jglr/parser-generator.js src/js/base/pyret-grammar.bnf build/phaseA/pyret-grammar.js "../../lib/jglr" "jglr/jglr" "pyret-base/js/pyret-parser"
	node build/phaseA/pyret-grammar.js src/arr/compiler/pyret-parser.js

clean:
	rm -r -f build/phaseA
	rm -f src/arr/compiler/pyret-parser.js
