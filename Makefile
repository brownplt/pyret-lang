PYRET_COMP = build/phase0/pyret.js
CLOSURE = java -jar deps/closure-compiler/compiler.jar
JS = js
JSBASE = $(JS)/base
JSTROVE = $(JS)/trove
BASE = arr/base
TROVE = arr/trove
COMPILER = arr/compiler

PHASE0 = build/phase0
PHASE1 = build/phase1
PHASE2 = build/phase2
PHASE3 = build/phase3
WEB = build/web

# CUSTOMIZE THESE IF NECESSARY
SRC_JS := $(patsubst %.arr,%.arr.js,$(wildcard src/$(COMPILER)/*.arr))
ROOT_LIBS = $(patsubst src/arr/base/%.arr,src/trove/%.js,$(wildcard src/$(BASE)/*.arr))
LIBS_JS := $(patsubst src/arr/trove/%.arr,src/trove/%.js,$(wildcard src/$(TROVE)/*.arr)) # deliberately .js suffix
PARSERS := $(patsubst src/js/base/%-grammar.bnf,src/js/%-parser-comp.js,$(wildcard src/$(JSBASE)/*-grammar.bnf))



COPY_JS = $(patsubst src/js/base/%.js,src/js/%.js,$(wildcard src/$(JSBASE)/*.js)) \
	src/js/js-numbers.js
TROVE_JS = $(patsubst src/js/trove/%.js,src/trove/%.js,$(wildcard src/$(JSTROVE)/*.js))

PHASE1_ALL_DEPS := $(patsubst src/%,$(PHASE1)/%,$(SRC_JS) $(ROOT_LIBS) $(LIBS_JS) $(COPY_JS) $(TROVE_JS))

PHASE2_ALL_DEPS := $(patsubst src/%,$(PHASE2)/%,$(SRC_JS) $(ROOT_LIBS) $(LIBS_JS) $(COPY_JS) $(TROVE_JS))

PHASE3_ALL_DEPS := $(patsubst src/%,$(PHASE3)/%,$(SRC_JS) $(ROOT_LIBS) $(LIBS_JS) $(COPY_JS) $(TROVE_JS))


PHASE1_DIRS := $(sort $(dir $(PHASE1_ALL_DEPS)))
PHASE2_DIRS := $(sort $(dir $(PHASE2_ALL_DEPS)))
PHASE3_DIRS := $(sort $(dir $(PHASE3_ALL_DEPS)))

# NOTE: Needs TWO blank lines here, dunno why
define \n


endef
ifneq ($(findstring .exe,$(SHELL)),)
	override SHELL:=$(COMSPEC)$(ComSpec)
	MKDIR = $(foreach dir,$1,if not exist "$(dir)". (md "$(dir)".)$(\n))
	RMDIR = $(foreach dir,$1,if exist "$(dir)". (rd /S /Q "$(dir)".)$(\n))
	RM = if exist "$1". (del $1)
else
	MKDIR = mkdir -p $1
	RMDIR = rm -rf $1
	RM = rm -f $1
endif

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
.PHONY : phase1
phase1: $(PYRET_COMP) $(PHASE1_ALL_DEPS) $(patsubst src/%,$(PHASE1)/%,$(PARSERS)) $(PHASE1)/pyret-start.js $(PHASE1)/main-wrapper.js

.PHONY : phase2
phase2: $(PYRET_COMP) $(PHASE2_ALL_DEPS) $(patsubst src/%,$(PHASE2)/%,$(PARSERS)) $(PHASE2)/pyret-start.js $(PHASE2)/main-wrapper.js

.PHONY : phase3
phase3: $(PYRET_COMP) $(PHASE3_ALL_DEPS) $(patsubst src/%,$(PHASE3)/%,$(PARSERS)) $(PHASE3)/pyret-start.js $(PHASE3)/main-wrapper.js


$(PHASE1_ALL_DEPS): | $(PHASE1)

$(PHASE2_ALL_DEPS): | $(PHASE2) phase1

$(PHASE3_ALL_DEPS): | $(PHASE3) phase2

.PHONY : standalone1
standalone1: phase1 $(PHASE1)/pyret.js

.PHONY : standalone2
standalone2: phase2 $(PHASE2)/pyret.js

.PHONY : standalone3
standalone3: phase3 $(PHASE3)/pyret.js

.PHONY : web
web: $(WEB_TARGETS) $(WEB)/web-compile.js

$(WEB_TARGETS): | $(WEB)

$(WEB):
	@$(call MKDIR,$(WEB))
$(WEB)/%: lib/CodeMirror/%
	cp $< $@
$(WEB)/%: src/web/%
	cp $< $@
$(WEB)/%: img/%
	cp $< $@

$(WEB)/web-compile.js: $(PHASE2_ALL_DEPS) $(patsubst src/%,$(PHASE2)/%,$(PARSERS))
	cd $(PHASE2) && \
	node ../../node_modules/requirejs/bin/r.js -o optimize=none baseUrl=. name=arr/compiler/web-compile.arr out=../web/web-compile.js paths.trove=trove include=js/runtime-anf

$(PHASE1):
	@$(call MKDIR,$(PHASE1_DIRS))

$(PHASE2):
	@$(call MKDIR,$(PHASE2_DIRS))

$(PHASE3):
	@$(call MKDIR,$(PHASE3_DIRS))

$(PHASE1)/pyret.js: $(PHASE1_ALL_DEPS) $(PHASE1)/pyret-start.js
	cd $(PHASE1) && \
		node ../../node_modules/requirejs/bin/r.js -o ../../src/scripts/require-build.js baseUrl=. name=pyret-start out=pyret.js paths.trove=trove

$(PHASE2)/pyret.js: $(PHASE2_ALL_DEPS) $(PHASE2)/pyret-start.js
	cd $(PHASE2) && \
		node ../../node_modules/requirejs/bin/r.js -o ../../src/scripts/require-build.js baseUrl=. name=pyret-start out=pyret.js paths.trove=trove

$(PHASE3)/pyret.js: $(PHASE3_ALL_DEPS) $(PHASE3)/pyret-start.js
	cd $(PHASE3) && \
		node ../../node_modules/requirejs/bin/r.js -o ../../src/scripts/require-build.js baseUrl=. name=pyret-start out=pyret.js paths.trove=trove

$(PHASE1)/pyret-start.js: src/scripts/pyret-start.js
	cp $< $@

$(PHASE2)/pyret-start.js: src/scripts/pyret-start.js
	cp $< $@

$(PHASE3)/pyret-start.js: src/scripts/pyret-start.js
	cp $< $@

$(PHASE1)/js/js-numbers.js: lib/js-numbers/src/js-numbers.js
	cp $< $@

$(PHASE2)/js/js-numbers.js: lib/js-numbers/src/js-numbers.js
	cp $< $@

$(PHASE3)/js/js-numbers.js: lib/js-numbers/src/js-numbers.js
	cp $< $@

$(PHASE1)/main-wrapper.js: src/scripts/main-wrapper.js
	cp $< $@

$(PHASE2)/main-wrapper.js: src/scripts/main-wrapper.js
	cp $< $@

$(PHASE3)/main-wrapper.js: src/scripts/main-wrapper.js
	cp $< $@

$(PHASE1)/$(JS)/%-parser-comp.js: src/$(JSBASE)/%-grammar.bnf $(wildcard lib/jglr/*.js)
	node lib/jglr/parser-generator.js src/$(JSBASE)/$*-grammar.bnf $(PHASE1)/$(JS)/$*-grammar.js
	node $(PHASE1)/$(JS)/$*-grammar.js $(PHASE1)/$(JS)/$*-parser.js
	$(CLOSURE) --js $(PHASE1)/$(JS)/$*-parser.js --js_output_file $(PHASE1)/$(JS)/$*-parser-comp.js --warning_level VERBOSE --externs src/scripts/externs.env --accept_const_keyword

$(PHASE2)/$(JS)/%-parser-comp.js: src/$(JSBASE)/%-grammar.bnf $(wildcard lib/jglr/*.js)
	node lib/jglr/parser-generator.js src/$(JSBASE)/$*-grammar.bnf $(PHASE2)/$(JS)/$*-grammar.js
	node $(PHASE2)/$(JS)/$*-grammar.js $(PHASE2)/$(JS)/$*-parser.js
	$(CLOSURE) --js $(PHASE2)/$(JS)/$*-parser.js --js_output_file $(PHASE2)/$(JS)/$*-parser-comp.js --warning_level VERBOSE --externs src/scripts/externs.env --accept_const_keyword

$(PHASE3)/$(JS)/%-parser-comp.js: src/$(JSBASE)/%-grammar.bnf $(wildcard lib/jglr/*.js)
	node lib/jglr/parser-generator.js src/$(JSBASE)/$*-grammar.bnf $(PHASE3)/$(JS)/$*-grammar.js
	node $(PHASE3)/$(JS)/$*-grammar.js $(PHASE3)/$(JS)/$*-parser.js
	$(CLOSURE) --js $(PHASE3)/$(JS)/$*-parser.js --js_output_file $(PHASE3)/$(JS)/$*-parser-comp.js --warning_level VERBOSE --externs src/scripts/externs.env --accept_const_keyword

$(PHASE1)/$(JS)/%.js : src/$(JSBASE)/%.js
	cp $< $@

$(PHASE2)/$(JS)/%.js : src/$(JSBASE)/%.js
	cp $< $@

$(PHASE3)/$(JS)/%.js : src/$(JSBASE)/%.js
	cp $< $@

$(PHASE1)/trove/%.js : src/$(JSTROVE)/%.js
	cp $< $@

$(PHASE2)/trove/%.js : src/$(JSTROVE)/%.js
	cp $< $@

$(PHASE3)/trove/%.js : src/$(JSTROVE)/%.js
	cp $< $@

$(PHASE1)/$(COMPILER)/%.arr.js : src/$(COMPILER)/%.arr $(PYRET_COMP)
	node $(PHASE0)/main-wrapper.js --compile-module-js $< > $@

$(PHASE2)/$(COMPILER)/%.arr.js : src/$(COMPILER)/%.arr $(PHASE1_ALL_DEPS)
	node $(PHASE1)/main-wrapper.js --compile-module-js $< > $@

$(PHASE3)/$(COMPILER)/%.arr.js : src/$(COMPILER)/%.arr $(PHASE2_ALL_DEPS)
	node $(PHASE2)/main-wrapper.js --compile-module-js $< > $@

$(PHASE1)/trove/%.js: src/$(BASE)/%.arr $(PYRET_COMP)
	node $(PHASE0)/main-wrapper.js --compile-module-js $< -library > $@

$(PHASE2)/trove/%.js: src/$(BASE)/%.arr $(PHASE1_ALL_DEPS)
	node $(PHASE1)/main-wrapper.js --compile-module-js $< -library > $@

$(PHASE3)/trove/%.js: src/$(BASE)/%.arr $(PHASE2_ALL_DEPS)
	node $(PHASE2)/main-wrapper.js --compile-module-js $< -library > $@

$(PHASE1)/trove/%.js: src/$(TROVE)/%.arr $(PYRET_COMP)
	node $(PHASE0)/main-wrapper.js --compile-module-js $< > $@

$(PHASE2)/trove/%.js: src/$(TROVE)/%.arr $(PHASE1_ALL_DEPS)
	node $(PHASE1)/main-wrapper.js --compile-module-js $< > $@

$(PHASE3)/trove/%.js: src/$(TROVE)/%.arr $(PHASE2_ALL_DEPS)
	node $(PHASE2)/main-wrapper.js --compile-module-js $< > $@

.PHONY : install
install:
	@$(call MKDIR,deps/closure-compiler)
	curl "http://dl.google.com/closure-compiler/compiler-latest.zip" > compiler-latest.zip
	unzip compiler-latest.zip -d deps/closure-compiler 
	@$(call RM,compiler-latest.zip)
	@$(call MKDIR,node_modules)
	npm install jasmine-node
	npm install requirejs
	npm install q
	npm install ses
	git submodule init
	git submodule update lib/CodeMirror


.PHONY : test
test: runtime-test evaluator-test compiler-test repl-test pyret-test

.PHONY : runtime-test
runtime-test : phase1
	cd tests/runtime/ && node test.js require-test-runner/

.PHONY : evaluator-test
evaluator-test: phase1
	cd tests/evaluator/ && node test.js require-test-runner/

.PHONY : repl-test
repl-test: phase1
	cd tests/repl/ && node test.js require-test-runner/

TEST_JS := $(patsubst tests/pyret/tests/%.arr,tests/pyret/tests/%.arr.js,$(wildcard tests/pyret/tests/*.arr))

tests/pyret/tests/%.arr.js: tests/pyret/tests/%.arr
	node $(PHASE1)/main-wrapper.js --compile-module-js $< > $@

.PHONY : pyret-test
pyret-test: phase1 $(TEST_JS)
	node build/phase1/main-wrapper.js \
    --module-load-dir tests/pyret \
    -check-all tests/pyret/main.arr

.PHONY : compiler-test
compiler-test: phase1
	node build/phase1/main-wrapper.js \
    --module-load-dir build/phase1/arr/compiler/ \
    -check-all src/arr/compiler/compile.arr

.PHONY : clean
clean:
	$(call RMDIR,$(PHASE1))
	$(call RMDIR,$(PHASE2))
	$(call RMDIR,$(PHASE3))


# Written this way because cmd.exe complains about && in command lines
new-bootstrap: no-diff-standalone
	sed "s/define('pyret-start/define('pyret/" $(PHASE2)/pyret.js > $(PHASE0)/pyret.js
no-diff-standalone: standalone2 standalone3
	diff $(PHASE2)/pyret.js $(PHASE3)/pyret.js
