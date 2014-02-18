PYRET_COMP = build/phase0/pyret.js
CLOSURE = java -jar deps/closure-compiler/compiler.jar
SWEETENER = node node_modules/sweet.js/bin/sjs
JS = js
JSBASE = $(JS)/base
JSSWEET = $(JS)/sweet
JSTROVE = $(JS)/trove
BASE = arr/base
TROVE = arr/trove
COMPILER = arr/compiler

PHASE0 = build/phase0
PHASE1 = build/phase1
PHASE2 = build/phase2
WEB = build/web

PYRET_PARSER1 = $(PHASE1)/$(JS)/pyret-parser-comp.js
PYRET_PARSER2 = $(PHASE2)/$(JS)/pyret-parser-comp.js

# CUSTOMIZE THESE IF NECESSARY
SRC_JS := $(patsubst %.arr,%.arr.js,$(wildcard src/$(COMPILER)/*.arr))
ROOT_LIBS = $(patsubst src/arr/base/%.arr,src/trove/%.js,$(wildcard src/$(BASE)/*.arr))
LIBS_JS := $(patsubst src/arr/trove/%.arr,src/trove/%.js,$(wildcard src/$(TROVE)/*.arr)) # deliberately .js suffix

COPY_JS = $(patsubst src/js/base/%.js,src/js/%.js,$(wildcard src/$(JSBASE)/*.js))
TROVE_JS = $(patsubst src/js/trove/%.js,src/trove/%.js,$(wildcard src/$(JSTROVE)/*.js))
MACRO_JS := $(patsubst src/js/sweet/%.js,src/js/%.js,$(wildcard src/$(JSSWEET)/*.js))

PHASE1_ALL_DEPS := $(patsubst src/%,$(PHASE1)/%,$(SRC_JS) $(MACRO_JS) $(ROOT_LIBS) $(LIBS_JS) $(COPY_JS) $(TROVE_JS))

PHASE2_ALL_DEPS := $(patsubst src/%,$(PHASE2)/%,$(SRC_JS) $(MACRO_JS) $(ROOT_LIBS) $(LIBS_JS) $(COPY_JS) $(TROVE_JS))

WEB_DEPS = \
 node_modules/requirejs/require.js \
 src/web/playground.html \
 lib/CodeMirror/lib/codemirror.css \
 lib/CodeMirror/lib/codemirror.js \
 lib/CodeMirror/mode/pyret.js \
 img/pyret-banner.png


WEB_TARGETS = $(addprefix build/web/,$(notdir $(WEB_DEPS)))

# Make sure that if a compilation step fails, we don't leave an empty but timestamp-up-to-date file
# laying (and lying) around to confuse future make
.DELETE_ON_ERROR:

# MAIN TARGET
phase1: $(PYRET_COMP) $(PHASE1_ALL_DEPS) $(PYRET_PARSER1) src/scripts/pyret-start.js $(PHASE1)/main-wrapper.js

phase2: $(PYRET_COMP) $(PHASE2_ALL_DEPS) $(PYRET_PARSER2) src/scripts/pyret-start.js $(PHASE2)/main-wrapper.js


$(PHASE1_ALL_DEPS): | $(PHASE1)

$(PHASE2_ALL_DEPS): | $(PHASE2) $(PHASE1)/main-wrapper.js

standalone1: phase1 $(PHASE1)/pyret.js

standalone2: standalone1 phase2 $(PHASE2)/pyret.js

web: $(WEB_TARGETS) $(WEB)/web-compile.js

$(WEB_TARGETS): | $(WEB)

$(WEB):
	mkdir -p $(WEB)
$(WEB)/%: lib/CodeMirror/%
	cp $< $@
$(WEB)/%: src/web/%
	cp $< $@
$(WEB)/%: img/%
	cp $< $@

$(WEB)/web-compile.js: $(PHASE2_ALL_DEPS)
	cd $(PHASE2) && \
	node ../../node_modules/requirejs/bin/r.js -o optimize=none baseUrl=. name=arr/compiler/web-compile.arr out=../web/web-compile.js paths.trove=trove include=js/runtime-anf

$(PHASE1):
	mkdir -p build/phase1
	mkdir -p build/phase1/trove
	cd build/phase1 && \
		find ../../src/ -type d | cut -d'/' -f4- | xargs mkdir -p

$(PHASE2):
	mkdir -p build/phase2
	mkdir -p build/phase2/trove
	cd build/phase2 && \
		find ../../src/ -type d | cut -d'/' -f4- | xargs mkdir -p

$(PHASE1)/pyret.js: $(PHASE1_ALL_DEPS) $(PHASE1)/pyret-start.js
	cd $(PHASE1) && \
		node ../../node_modules/requirejs/bin/r.js -o ../../src/scripts/require-build.js baseUrl=. name=pyret-start out=pyret.js paths.trove=trove

$(PHASE2)/pyret.js: $(PHASE2_ALL_DEPS) $(PHASE2)/pyret-start.js
	cd $(PHASE2) && \
		node ../../node_modules/requirejs/bin/r.js -o ../../src/scripts/require-build.js baseUrl=. name=pyret-start out=pyret.js paths.trove=trove

$(PHASE1)/pyret-start.js: src/scripts/pyret-start.js
	cp $< $@

$(PHASE2)/pyret-start.js: src/scripts/pyret-start.js
	cp $< $@

$(PHASE1)/main-wrapper.js: src/scripts/main-wrapper.js
	cp $< $@

$(PHASE2)/main-wrapper.js: src/scripts/main-wrapper.js
	cp $< $@

$(PYRET_PARSER1): lib/jglr/parser-generator.js src/$(JSBASE)/pyret-grammar.bnf
	node lib/jglr/parser-generator.js src/$(JSBASE)/pyret-grammar.bnf $(PHASE1)/$(JS)/grammar.js
	node $(PHASE1)/$(JS)/grammar.js $(PHASE1)/$(JS)/pyret-parser.js
	$(CLOSURE) --js $(PHASE1)/$(JS)/pyret-parser.js --js_output_file $(PHASE1)/$(JS)/pyret-parser-comp.js --warning_level VERBOSE --externs src/scripts/externs.env --accept_const_keyword

$(PYRET_PARSER2): lib/jglr/parser-generator.js src/$(JSBASE)/pyret-grammar.bnf
	node lib/jglr/parser-generator.js src/$(JSBASE)/pyret-grammar.bnf $(PHASE2)/$(JS)/grammar.js
	node $(PHASE2)/$(JS)/grammar.js $(PHASE2)/$(JS)/pyret-parser.js
	$(CLOSURE) --js $(PHASE2)/$(JS)/pyret-parser.js --js_output_file $(PHASE2)/$(JS)/pyret-parser-comp.js --warning_level VERBOSE --externs src/scripts/externs.env --accept_const_keyword

$(PHASE1)/$(JS)/%.js: src/$(JSSWEET)/%.js
	$(SWEETENER) -o $@ $< -m ./src/scripts/macros.js
	$(CLOSURE) --js $@ --externs src/scripts/externs.env > /dev/null

$(PHASE2)/$(JS)/%.js: src/$(JSSWEET)/%.js
	$(SWEETENER) -o $@ $< -m ./src/scripts/macros.js
	$(CLOSURE) --js $@ --externs src/scripts/externs.env > /dev/null

$(PHASE1)/$(JS)/%.js : src/$(JSBASE)/%.js
	cp $< $@

$(PHASE2)/$(JS)/%.js : src/$(JSBASE)/%.js
	cp $< $@

$(PHASE1)/trove/%.js : src/$(JSTROVE)/%.js
	cp $< $@

$(PHASE2)/trove/%.js : src/$(JSTROVE)/%.js
	cp $< $@

$(PHASE1)/$(COMPILER)/%.arr.js : src/$(COMPILER)/%.arr $(PYRET_COMP)
	node $(PHASE0)/main-wrapper.js --compile-module-js $< > $@

$(PHASE2)/$(COMPILER)/%.arr.js : src/$(COMPILER)/%.arr $(PHASE1)/pyret.js
	node $(PHASE1)/main-wrapper.js --compile-module-js $< > $@

$(PHASE1)/trove/%.js: src/$(BASE)/%.arr $(PYRET_COMP)
	node $(PHASE0)/main-wrapper.js --compile-module-js $< -library > $@

$(PHASE2)/trove/%.js: src/$(BASE)/%.arr $(PHASE1)/pyret.js
	node $(PHASE1)/main-wrapper.js --compile-module-js $< -library > $@

$(PHASE1)/trove/%.js: src/$(TROVE)/%.arr $(PYRET_COMP)
	node $(PHASE0)/main-wrapper.js --compile-module-js $< > $@

$(PHASE2)/trove/%.js: src/$(TROVE)/%.arr $(PHASE1)/pyret.js
	node $(PHASE1)/main-wrapper.js --compile-module-js $< > $@

install:
	mkdir -p deps/closure-compiler
	wget "http://dl.google.com/closure-compiler/compiler-latest.zip"
	unzip compiler-latest.zip -d deps/closure-compiler 
	rm compiler-latest.zip
	mkdir node_modules -p
	npm install jasmine-node
	npm install sweet.js
	npm install requirejs


test: runtime-test

RUNTIME_JS = $(patsubst src/%,$(PHASE2)/%,$(MACRO_JS) $(COPY_JS))

.PHONY : runtime-test
runtime-test : $(RUNTIME_JS)
	cd tests/runtime/ && node test.js require-test-runner/


.PHONY : clean
clean:
	rm -rf $(PHASE1)
	rm -rf $(PHASE2)


