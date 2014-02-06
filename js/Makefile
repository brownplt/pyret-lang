PYRET_COMP = standalone-compiler/pyret.js
COMPILER = java -jar closure-compiler/compiler.jar
SWEETENER = node node_modules/sweet.js/bin/sjs
BI = builtin-libs
RACKET_FFI = ../src/lang/racket-ffi
PYRET_PARSER = $(BI)/pyret-parser-comp.js

# CUSTOMIZE THESE IF NECESSARY
SRC_JS := $(patsubst %.arr,%.arr.js,$(wildcard *.arr)) $(patsubst %-raw.js,%.js,$(wildcard *-raw.js))
ROOT_LIBS_JS = $(BI)/list.js $(BI)/option.js $(BI)/error.js $(BI)/sets.js
LIBS_JS := $(patsubst %.arr,%.js,$(wildcard $(BI)/*.arr)) $(wildcard $(BI)/*.js)

COPY_LIBS = $(RACKET_FFI)/pprint.rkt $(RACKET_FFI)/cmdline.rkt $(RACKET_FFI)/ast.arr $(RACKET_FFI)/file.rkt
COPY_LIBS_JS = $(patsubst %.arr,%.js,$(patsubst %.rkt,%.js,$(patsubst $(RACKET_FFI)/%,$(BI)/%, $(COPY_LIBS))))

COMP_JS = runtime-anf-comp.js namespace-comp.js

# MAIN TARGET
build: $(PYRET_COMP) $(SRC_JS) $(ROOT_LIBS_JS) $(LIBS_JS) $(COPY_LIBS_JS) $(COMP_JS) pyret.js

standalone: build standalone-compiler/pyret-built.js



standalone-compiler/pyret-built.js: $(PYRET_COMP) \
		$(SRC_JS) $(ROOT_LIBS_JS) $(LIBS_JS) $(COPY_LIBS_JS) $(COMP_JS) pyret.js
	node node_modules/requirejs/bin/r.js -o require-build.js

$(PYRET_PARSER): bnf-grammar.js grammar-full.rkt
	node bnf-grammar.js grammar-full.rkt $(BI)/grammar.js
	node $(BI)/grammar.js $(BI)/pyret-parser.js
	$(COMPILER) --js $(BI)/pyret-parser.js --js_output_file $(BI)/pyret-parser-comp.js --warning_level VERBOSE --externs externs.js --accept_const_keyword

%.js: %-raw.js $(PYRET_COMP)
	$(SWEETENER) -o $*.sweet.js $< -m ./macros.js
	$(COMPILER) --js $*.sweet.js --js_output_file $@ --externs externs.js
	rm $*.sweet.js
%.arr.js : %.arr $(PYRET_COMP)
	node standalone-compiler/main-wrapper.js --compile-module-js $< > $@

$(BI)/%.js : $(BI)/%.arr $(PYRET_COMP)
	node standalone-compiler/main-wrapper.js --compile-module-js $< > $@
$(BI)/pprint.arr: $(RACKET_FFI)/pprint.rkt
	cp $< $@
$(BI)/cmdline.arr: $(RACKET_FFI)/cmdline.rkt
	cp $< $@
$(BI)/ast.arr: $(RACKET_FFI)/ast.arr
	cp $< $@
$(BI)/file.arr: $(RACKET_FFI)/file.rkt
	cp $< $@

%-comp.js: %.js $(PYRET_COMP)
	$(COMPILER) --js $< --js_output_file $@ --warning_level VERBOSE --externs externs.js


$(BI)/list.js: $(BI)/list.arr
	node standalone-compiler/main-wrapper.js --compile-module-js $< -library > $@
$(BI)/option.js: $(BI)/option.arr
	node standalone-compiler/main-wrapper.js --compile-module-js $< -library > $@
$(BI)/error.js: $(BI)/error.arr
	node standalone-compiler/main-wrapper.js --compile-module-js $< -library > $@
$(BI)/sets.js: $(BI)/sets.arr
	node standalone-compiler/main-wrapper.js --compile-module-js $< -library > $@

## HACK UNTIL WE HANDLE TRY/CATCH
create-tests.arr.js : create-tests.arr
	touch create-tests.arr.js

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
	rm $(PYRET_PARSER) standalone-compiler/pyret-built.js $(SRC_JS) $(ROOT_LIBS_JS) $(LIBS_JS) $(COPY_LIBS_JS) $(COMP_JS) 
