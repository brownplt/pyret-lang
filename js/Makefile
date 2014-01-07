compile: runtime-anf.js
	java -jar closure-compiler/compiler.jar --js runtime-anf.js --js_output_file anf-comp.js --warning_level VERBOSE

install:
	mkdir -p closure-compiler
	wget "http://dl.google.com/closure-compiler/compiler-latest.zip"
	unzip compiler-latest.zip -d closure-compiler 
	rm compiler-latest.zip
	npm install jasmine-node
	mkdir compiled

test:
	mkdir -p generated-tests/anf-tests
	node test.js
