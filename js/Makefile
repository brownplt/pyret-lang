build: sweeten compile compiler

compiler:
	raco make pyret.arr
	bash copy-libs.sh
	bash compile-libs.sh
	bash compile-compiler.sh
	./node_modules/.bin/r.js -o require-build.js

COMPILER = java -jar closure-compiler/compiler.jar
SWEETENER = node node_modules/sweet.js/bin/sjs
BI = builtin-libs
PYRET_PARSER = $(BI)/pyret-parser-comp.js

#Compiles/Minimizes the Files
#Performs type checking using JSDoc's
compile: runtime-anf.js number-dict-sweet.js namespace.js string-dict-sweet.js $(PYRET_PARSER)
	$(COMPILER) --js runtime-anf.js --js_output_file anf-comp.js --warning_level VERBOSE --externs externs.js
	$(COMPILER) --js namespace.js --js_output_file namespace-comp.js --warning_level VERBOSE --externs externs.js
	$(COMPILER) --js number-dict-sweet.js --js_output_file number-dict.js --externs externs.js
	$(COMPILER) --js string-dict-sweet.js --js_output_file string-dict.js --externs externs.js
	$(COMPILER) --js boolean-dict-sweet.js --js_output_file boolean-dict.js --externs externs.js

$(PYRET_PARSER): bnf-grammar.js grammar-full.rkt
	node bnf-grammar.js grammar-full.rkt $(BI)/grammar.js
	node $(BI)/grammar.js $(BI)/pyret-parser.js
	$(COMPILER) --js $(BI)/pyret-parser.js --js_output_file $(BI)/pyret-parser-comp.js --warning_level VERBOSE --externs externs.js --accept_const_keyword

#Expand all macros
sweeten: number-dict-raw.js string-dict-raw.js boolean-dict-raw.js
	$(SWEETENER) -o number-dict-sweet.js number-dict-raw.js -m ./macros.js
	$(SWEETENER) -o string-dict-sweet.js string-dict-raw.js -m ./macros.js
	$(SWEETENER) -o boolean-dict-sweet.js boolean-dict-raw.js -m ./macros.js

install:
	mkdir -p closure-compiler
	wget "http://dl.google.com/closure-compiler/compiler-latest.zip"
	unzip compiler-latest.zip -d closure-compiler 
	rm compiler-latest.zip
	mkdir node_modules -p
	npm install jasmine-node
	npm install sweet.js
	npm install requirejs

.PHONY : test
test: runtime-anf.js number-dict.js
	mkdir -p generated-tests/anf-tests
	node node_modules/jasmine-node/bin/jasmine-node test --verbose

.PHONY : clean
clean:
	rm number-dict.js number-dict-sweet.js string-dict.js string-dict-sweet.js boolean-dict.js boolean-dict-sweet.js anf-comp.js namespace-comp.js $(BI)/pyret-parser.js $(BI)/grammar.js $(BI)/pyret-parser-comp.js
