build: sweeten compile

COMPILER = java -jar closure-compiler/compiler.jar
SWEETENER = node node_modules/sweet.js/bin/sjs

#Compiles/Minimizes the Files
#Performs type checking using JSDoc's
compile: runtime-anf.js number-dict-sweet.js namespace.js string-dict-sweet.js
	$(COMPILER) --js runtime-anf.js --js_output_file anf-comp.js --warning_level VERBOSE --externs externs.js
	$(COMPILER) --js namespace.js --js_output_file namespace-comp.js --warning_level VERBOSE --externs externs.js
	$(COMPILER) --js number-dict-sweet.js --js_output_file number-dict.js --externs externs.js
	$(COMPILER) --js string-dict-sweet.js --js_output_file string-dict.js --externs externs.js

#Expand all macros
sweeten: number-dict-raw.js string-dict-raw.js
	$(SWEETENER) -o number-dict-sweet.js number-dict-raw.js -m ./macros.js
	$(SWEETENER) -o string-dict-sweet.js string-dict-raw.js -m ./macros.js

install:
	mkdir -p closure-compiler
	wget "http://dl.google.com/closure-compiler/compiler-latest.zip"
	unzip compiler-latest.zip -d closure-compiler 
	rm compiler-latest.zip
	npm install jasmine-node
	npm install sweet.js

test: runtime-anf.js number-dict.js
	mkdir -p generated-tests/anf-tests
	node test.js test
