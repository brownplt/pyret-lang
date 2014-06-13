PYRET_COMP       = build/phase0/pyret.js
CLOSURE          = java -jar deps/closure-compiler/compiler.jar
NODE             = node -max-old-space-size=8192
JS               = js
JSBASE           = $(JS)/base
JSTROVE          = $(JS)/trove
BASE             = arr/base
TROVE            = arr/trove
COMPILER         = arr/compiler

PHASE0           = build/phase0
PHASE1           = build/phase1
PHASE2           = build/phase2
PHASE3           = build/phase3
WEB              = build/web
RELEASE_DIR      = build/release
DOCS             = docs

# CUSTOMIZE THESE IF NECESSARY
SRC_JS          := $(patsubst %.arr,%.arr.js,$(wildcard src/$(COMPILER)/*.arr))
ROOT_LIBS        = $(patsubst src/arr/base/%.arr,src/trove/%.js,$(wildcard src/$(BASE)/*.arr))
LIBS_JS         := $(patsubst src/arr/trove/%.arr,src/trove/%.js,$(wildcard src/$(TROVE)/*.arr)) # deliberately .js suffix
PARSERS         := $(patsubst src/js/base/%-grammar.bnf,src/js/%-parser.js,$(wildcard src/$(JSBASE)/*-grammar.bnf))

# You can download the script to work with s3 here:
# 
#     http://aws.amazon.com/code/Amazon-S3/1710
#
# On Debian, you need the following packages:
#
#  - libterm-shellui-perl
#  - liblog-log4perl-perl
#  - libnet-amazon-s3-perl
#  - libnet-amazon-perl
#  - libnet-amazon-s3-tools-perl
#  - parallel
#
# You will then need to place your AWS id and secret in ~/.aws, in the
# following format:
#
#     id     = <your aws id>
#     secret = <your aws secret>
#
# Make sure that the s3 script is in your PATH, or modify the value
# below.
S3               = s3

COPY_JS          = $(patsubst src/js/base/%.js,src/js/%.js,$(wildcard src/$(JSBASE)/*.js)) \
	src/js/js-numbers.js
TROVE_JS         = $(patsubst src/js/trove/%.js,src/trove/%.js,$(wildcard src/$(JSTROVE)/*.js))

PHASE1_ALL_DEPS := $(patsubst src/%,$(PHASE1)/%,$(LIBS_JS) $(ROOT_LIBS) $(TROVE_JS) $(SRC_JS) $(COPY_JS))

PHASE2_ALL_DEPS := $(patsubst src/%,$(PHASE2)/%,$(LIBS_JS) $(ROOT_LIBS) $(TROVE_JS) $(SRC_JS) $(COPY_JS))

PHASE3_ALL_DEPS := $(patsubst src/%,$(PHASE3)/%,$(LIBS_JS) $(ROOT_LIBS) $(TROVE_JS) $(SRC_JS) $(COPY_JS))

DOCS_DEPS        = $(patsubst src/%,$(DOCS)/generated/%.rkt,$(SRC_JS) $(TROVE_JS) $(LIBS_JS) $(COPY_JS) $(ROOT_LIBS))
DOCS_SKEL_DEPS   = $(patsubst src/%,$(DOCS)/skeleton/%.rkt,$(SRC_JS) $(LIBS_JS) $(ROOT_LIBS))

PHASE1_DIRS     := $(sort $(dir $(PHASE1_ALL_DEPS)))
PHASE2_DIRS     := $(sort $(dir $(PHASE2_ALL_DEPS)))
PHASE3_DIRS     := $(sort $(dir $(PHASE3_ALL_DEPS)))
DOCS_DIRS       := $(sort $(dir $(DOCS_DEPS)) $(dir $(DOCS_SKEL_DEPS)))

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
	VERSION = $(shell git describe --long --tags HEAD | awk -F '[/-]' '{ print $$1 "r" $$2 }')
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
phase1: $(PHASE1)/phase1.built

$(PHASE1)/phase1.built: $(PYRET_COMP) $(PHASE1_ALL_DEPS) $(patsubst src/%,$(PHASE1)/%,$(PARSERS)) $(PHASE1)/pyret-start.js $(PHASE1)/main-wrapper.js
	touch $(PHASE1)/phase1.built

.PHONY : phase2
phase2: $(PHASE2)/phase2.built

$(PHASE2)/phase2.built: $(PYRET_COMP) $(PHASE2_ALL_DEPS) $(patsubst src/%,$(PHASE2)/%,$(PARSERS)) $(PHASE2)/pyret-start.js $(PHASE2)/main-wrapper.js
	touch $(PHASE2)/phase2.built

.PHONY : phase3
phase3: $(PHASE3)/phase3.built

$(PHASE3)/phase3.built: $(PYRET_COMP) $(PHASE3_ALL_DEPS) $(patsubst src/%,$(PHASE3)/%,$(PARSERS)) $(PHASE3)/pyret-start.js $(PHASE3)/main-wrapper.js
	touch $(PHASE3)/phase3.built


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
	$(NODE) ../../node_modules/requirejs/bin/r.js -o optimize=none baseUrl=. name=arr/compiler/web-compile.arr out=../web/web-compile.js paths.trove=trove paths.compiler=arr/compiler include=js/runtime-anf include=js/repl-lib

$(PHASE1):
	@$(call MKDIR,$(PHASE1_DIRS))

$(PHASE2):
	@$(call MKDIR,$(PHASE2_DIRS))

$(PHASE3):
	@$(call MKDIR,$(PHASE3_DIRS))

$(PHASE1)/pyret.js: $(PHASE1_ALL_DEPS) $(PHASE1)/pyret-start.js
	cd $(PHASE1) && \
		$(NODE) ../../node_modules/requirejs/bin/r.js -o ../../src/scripts/require-build.js baseUrl=. name=pyret-start out=pyret.js paths.trove=trove paths.compiler=arr/compiler include=js/runtime-anf include=js/repl-lib


$(PHASE2)/pyret.js: $(PHASE2_ALL_DEPS) $(PHASE2)/pyret-start.js
	cd $(PHASE2) && \
		$(NODE) ../../node_modules/requirejs/bin/r.js -o ../../src/scripts/require-build.js baseUrl=. name=pyret-start out=pyret.js paths.trove=trove paths.compiler=arr/compiler include=js/runtime-anf include=js/repl-lib

$(PHASE3)/pyret.js: $(PHASE3_ALL_DEPS) $(PHASE3)/pyret-start.js
	cd $(PHASE3) && \
		$(NODE) ../../node_modules/requirejs/bin/r.js -o ../../src/scripts/require-build.js baseUrl=. name=pyret-start out=pyret.js paths.trove=trove paths.compiler=arr/compiler include=js/runtime-anf include=js/repl-lib

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

$(PHASE1)/$(JS)/%-parser.js: src/$(JSBASE)/%-grammar.bnf src/$(JSBASE)/%-tokenizer.js $(wildcard lib/jglr/*.js)
	$(NODE) lib/jglr/parser-generator.js src/$(JSBASE)/$*-grammar.bnf $(PHASE1)/$(JS)/$*-grammar.js
	$(NODE) $(PHASE1)/$(JS)/$*-grammar.js $(PHASE1)/$(JS)/$*-parser.js

$(PHASE2)/$(JS)/%-parser.js: src/$(JSBASE)/%-grammar.bnf src/$(JSBASE)/%-tokenizer.js $(wildcard lib/jglr/*.js)
	$(NODE) lib/jglr/parser-generator.js src/$(JSBASE)/$*-grammar.bnf $(PHASE2)/$(JS)/$*-grammar.js
	$(NODE) $(PHASE2)/$(JS)/$*-grammar.js $(PHASE2)/$(JS)/$*-parser.js

$(PHASE3)/$(JS)/%-parser.js: src/$(JSBASE)/%-grammar.bnf src/$(JSBASE)/%-tokenizer.js $(wildcard lib/jglr/*.js)
	$(NODE) lib/jglr/parser-generator.js src/$(JSBASE)/$*-grammar.bnf $(PHASE3)/$(JS)/$*-grammar.js
	$(NODE) $(PHASE3)/$(JS)/$*-grammar.js $(PHASE3)/$(JS)/$*-parser.js

$(PHASE1)/$(JS)/%.js : src/$(JSBASE)/%.js
	cp $< $@

.PHONY : docs
docs: $(DOCS_DEPS)

$(DOCS_DEPS): | $(PHASE1)/phase1.built docs-trove

docs-trove: $(DOCS)/doc-utils.arr.js
	@$(call MKDIR,$(DOCS_DIRS))

$(DOCS)/%.arr.js : $(DOCS)/%.arr $(PHASE1_ALL_DEPS)
	node $(PHASE1)/main-wrapper.js --compile-module-js $< > $@

$(DOCS)/generated/trove/%.js.rkt : src/$(JSTROVE)/%.js docs/create-js-generated-docs.js
	$(NODE) docs/create-js-generated-docs.js $(patsubst src/$(JSTROVE)/%,$(PHASE1)/trove/%,$<) > $@

$(DOCS)/generated/js/%.js.rkt : src/$(JSBASE)/%.js docs/create-js-generated-docs.js
	$(NODE) docs/create-js-generated-docs.js $(patsubst src/$(JSBASE)/%,$(PHASE1)/js/%,$<) > $@

$(DOCS)/generated/trove/%.js.rkt : src/$(TROVE)/%.arr docs/create-arr-generated-docs.arr
	$(NODE) $(PHASE1)/main-wrapper.js -no-check-mode docs/create-arr-generated-docs.arr $< $@
$(DOCS)/generated/trove/%.js.rkt : src/$(BASE)/%.arr docs/create-arr-generated-docs.arr
	$(NODE) $(PHASE1)/main-wrapper.js -no-check-mode docs/create-arr-generated-docs.arr $< $@
$(DOCS)/generated/arr/compiler/%.arr.js.rkt : src/$(COMPILER)/%.arr docs/create-arr-generated-docs.arr
	$(NODE) $(PHASE1)/main-wrapper.js -no-check-mode docs/create-arr-generated-docs.arr $< $@

docs-skel: $(DOCS_SKEL_DEPS)
$(DOCS_SKEL_DEPS): | $(PHASE1)/phase1.built docs-trove
$(DOCS)/written/trove/%.js.rkt : src/$(TROVE)/%.arr docs/create-arr-doc-skeleton.arr
	$(NODE) $(PHASE1)/main-wrapper.js -no-check-mode docs/create-arr-doc-skeleton.arr $< $@
$(DOCS)/written/trove/%.js.rkt : src/$(BASE)/%.arr docs/create-arr-doc-skeleton.arr
	$(NODE) $(PHASE1)/main-wrapper.js -no-check-mode docs/create-arr-doc-skeleton.arr $< $@
$(DOCS)/written/arr/compiler/%.arr.js.rkt : src/$(COMPILER)/%.arr docs/create-arr-doc-skeleton.arr
	$(NODE) $(PHASE1)/main-wrapper.js -no-check-mode docs/create-arr-doc-skeleton.arr $< $@


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
	$(NODE) $(PHASE0)/main-wrapper.js --compile-module-js $< > $@

$(PHASE2)/$(COMPILER)/%.arr.js : src/$(COMPILER)/%.arr $(PHASE1_ALL_DEPS)
	$(NODE) $(PHASE1)/main-wrapper.js --compile-module-js $< > $@

$(PHASE3)/$(COMPILER)/%.arr.js : src/$(COMPILER)/%.arr $(PHASE2_ALL_DEPS)
	$(NODE) $(PHASE2)/main-wrapper.js --compile-module-js $< > $@

$(PHASE1)/trove/%.js: src/$(BASE)/%.arr $(PYRET_COMP)
	$(NODE) $(PHASE0)/main-wrapper.js --compile-module-js $< -library > $@

$(PHASE2)/trove/%.js: src/$(BASE)/%.arr $(PHASE1_ALL_DEPS)
	$(NODE) $(PHASE1)/main-wrapper.js --compile-module-js $< -library > $@

$(PHASE3)/trove/%.js: src/$(BASE)/%.arr $(PHASE2_ALL_DEPS)
	$(NODE) $(PHASE2)/main-wrapper.js --compile-module-js $< -library > $@

$(PHASE1)/trove/%.js: src/$(TROVE)/%.arr $(PYRET_COMP)
	$(NODE) $(PHASE0)/main-wrapper.js --compile-module-js $< > $@

$(PHASE2)/trove/%.js: src/$(TROVE)/%.arr $(PHASE1_ALL_DEPS)
	$(NODE) $(PHASE1)/main-wrapper.js --compile-module-js $< > $@

$(PHASE3)/trove/%.js: src/$(TROVE)/%.arr $(PHASE2_ALL_DEPS)
	$(NODE) $(PHASE2)/main-wrapper.js --compile-module-js $< > $@

.PHONY : install
install:
	@$(call MKDIR,node_modules)
	npm install jasmine-node
	npm install requirejs
	npm install q
	npm install ses
	git submodule init
	git submodule update lib/CodeMirror


.PHONY : test
test: runtime-test evaluator-test compiler-test repl-test pyret-test bootstrap-test

.PHONY : runtime-test
runtime-test : $(PHASE1)/phase1.built
	cd tests/runtime/ && $(NODE) test.js require-test-runner/

.PHONY : evaluator-test
evaluator-test: $(PHASE1)/phase1.built
	cd tests/evaluator/ && $(NODE) test.js require-test-runner/

.PHONY : repl-test
repl-test: $(PHASE1)/phase1.built
	cd tests/repl/ && $(NODE) test.js require-test-runner/

.PHONY : parse-test
parse-test: $(PHASE1)/phase1.built
	cd tests/parse/ && node test.js require-test-runner/

TEST_JS := $(patsubst tests/pyret/tests/%.arr,tests/pyret/tests/%.arr.js,$(wildcard tests/pyret/tests/*.arr))
BS_TEST_JS := $(patsubst tests/pyret/bootstrap-tests/%.arr,tests/pyret/bootstrap-tests/%.arr.js,$(wildcard tests/pyret/bootstrap-tests/*.arr))

tests/pyret/tests/%.arr.js: tests/pyret/tests/%.arr $(PHASE1)/phase1.built
	$(NODE) $(PHASE1)/main-wrapper.js --compile-module-js $< > $@
tests/pyret/bootstrap-tests/%.arr.js: tests/pyret/bootstrap-tests/%.arr $(PHASE1)/phase1.built
	$(NODE) $(PHASE1)/main-wrapper.js --dialect Bootstrap --compile-module-js $< > $@

.PHONY : pyret-test
pyret-test: $(PHASE1)/phase1.built $(TEST_JS)
	$(NODE) $(PHASE1)/main-wrapper.js \
    --module-load-dir tests/pyret \
    -check-all tests/pyret/main.arr

.PHONY : compiler-test
compiler-test: $(PHASE1)/phase1.built
	$(NODE) $(PHASE1)/main-wrapper.js \
    --module-load-dir $(PHASE1)/arr/compiler/ \
    -check-all src/arr/compiler/compile.arr

.PHONY : bootstrap-test
bootstrap-test: $(PHASE1)/phase1.built $(BS_TEST_JS)
	$(NODE) $(PHASE1)/main-wrapper.js \
    --module-load-dir tests/pyret \
    --dialect Bootstrap \
    -check-all tests/pyret/bootstrap-main.arr \

.PHONY : clean
clean:
	$(call RMDIR,$(PHASE1))
	$(call RMDIR,$(PHASE2))
	$(call RMDIR,$(PHASE3))
	$(call RMDIR,$(RELEASE_DIR))


# Written this way because cmd.exe complains about && in command lines
new-bootstrap: no-diff-standalone
	sed "s/define('pyret-start/define('pyret/" $(PHASE2)/pyret.js > $(PHASE0)/pyret.js
no-diff-standalone: standalone2 standalone3
	diff $(PHASE2)/pyret.js $(PHASE3)/pyret.js


$(RELEASE_DIR)/phase1:
	$(call MKDIR,$(RELEASE_DIR)/phase1)

ifdef VERSION
release-gzip: $(PYRET_COMP) phase1 $(RELEASE_DIR)/phase1
	gzip -c $(PYRET_COMP) > $(RELEASE_DIR)/pyret.js
	(cd $(PHASE1) && find * -type d -print0) | parallel --gnu -0 mkdir -p '$(RELEASE_DIR)/phase1/{}'
	(cd $(PHASE1) && find * -type f -print0) | parallel --gnu -0 "gzip -c '$(PHASE1)/{}' > '$(RELEASE_DIR)/phase1/{}'"
# If you need information on using the s3 script, run `s3 --man'
release: release-gzip
	cd $(RELEASE_DIR) && \
	find * -type f -print0 | parallel --gnu -0 $(S3) add --header 'Content-Type:text/javascript' --header 'Content-Encoding:gzip' --acl 'public-read' ':pyret-releases/$(VERSION)/{}' '{}'
else
release-gzip:
	$(error Cannot release from this platform)
release:
	$(error Cannot release from this platform)
endif
