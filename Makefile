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
RELEASE_DIR      = build/release
DOCS             = docs

# CUSTOMIZE THESE IF NECESSARY
SRC_JS          := $(patsubst %.arr,%.arr.js,$(wildcard src/$(COMPILER)/*.arr))\
 $(patsubst %.arr,%.arr.js,$(wildcard src/$(COMPILER)/locators/*.arr))
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

-include config.mk

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

$(PHASE1)/js/js-numbers.js: src/js/base/js-numbers.js
	cp $< $@

$(PHASE2)/js/js-numbers.js: src/js/base/js-numbers.js
	cp $< $@

$(PHASE3)/js/js-numbers.js: src/js/base/js-numbers.js
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
docs:
	cd docs/written && make VERSION=$(VERSION)

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
	npm install

PYRET_TEST_PHASE=$(P)
ifeq ($(PYRET_TEST_PHASE),2)
  PYRET_TEST_PHASE=$(PHASE2)
  PYRET_TEST_PREREQ=$(PHASE2)/phase2.built
else
ifeq ($(PYRET_TEST_PHASE),3)
  PYRET_TEST_PHASE=$(PHASE3)
  PYRET_TEST_PREREQ=$(PHASE3)/phase3.built
else
  PYRET_TEST_PHASE=$(PHASE1)
  PYRET_TEST_PREREQ=$(PHASE1)/phase1.built
endif
endif

.PHONY : test
test: runtime-test evaluator-test compiler-test repl-test pyret-test regression-test type-check-test lib-test

.PHONY : test-all
test-all: test docs-test benchmark-test

.PHONY : runtime-test
runtime-test : $(PYRET_TEST_PREREQ)
	cd tests/runtime/ && PHASE=$(PYRET_TEST_PHASE) $(NODE) test.js require-test-runner/

.PHONY : evaluator-test
evaluator-test: $(PYRET_TEST_PREREQ)
	cd tests/evaluator/ && PHASE=$(PYRET_TEST_PHASE) $(NODE) test.js require-test-runner/

.PHONY : repl-test
repl-test: $(PYRET_TEST_PREREQ) tests/repl/repl.js
	cd tests/repl/ && PHASE=$(PYRET_TEST_PHASE) $(NODE) test.js require-test-runner/

.PHONY : parse-test
parse-test: tests/parse/parse.js build/phase1/js/pyret-tokenizer.js build/phase1/js/pyret-parser.js
	cd tests/parse/ && $(NODE) test.js require-test-runner/

TEST_HELP_JS := $(patsubst tests/pyret/%helper.arr,tests/pyret/%helper.arr.js,$(wildcard tests/pyret/*helper.arr))
TEST_JS := $(patsubst tests/pyret/tests/%.arr,tests/pyret/tests/%.arr.js,$(wildcard tests/pyret/tests/*.arr))
REGRESSION_TEST_JS := $(patsubst tests/pyret/regression/%.arr,tests/pyret/regression/%.arr.js,$(wildcard tests/pyret/regression/*.arr))

tests/pyret/%helper.arr.js: tests/pyret/%helper.arr
	$(NODE) $(PYRET_TEST_PHASE)/main-wrapper.js --compile-module-js $< > $@

tests/pyret/tests/%.arr.js: tests/pyret/tests/%.arr $(PYRET_TEST_PREREQ)
	$(NODE) $(PYRET_TEST_PHASE)/main-wrapper.js --compile-module-js $< > $@

tests/pyret/regression/%.arr.js: tests/pyret/regression/%.arr $(PYRET_TEST_PREREQ)
	$(NODE) $(PYRET_TEST_PHASE)/main-wrapper.js --compile-module-js $< > $@

.PHONY : regression-test
regression-test: $(PYRET_TEST_PREREQ) $(REGRESSION_TEST_JS) $(TEST_HELP_JS)
	$(NODE) $(PYRET_TEST_PHASE)/main-wrapper.js \
    --module-load-dir tests/pyret \
    -check-all tests/pyret/regression.arr

.PHONY : pyret-test
pyret-test: $(PYRET_TEST_PREREQ) $(TEST_JS) $(TEST_HELP_JS)
	$(NODE) $(PYRET_TEST_PHASE)/main-wrapper.js \
    --module-load-dir tests/pyret \
    -check-all tests/pyret/main.arr

.PHONY : type-check-test
type-check-test: $(PYRET_TEST_PREREQ) $(TEST_HELP_JS)
	$(NODE) $(PYRET_TEST_PHASE)/main-wrapper.js \
    --module-load-dir tests/type-check \
    -check-all tests/type-check/main.arr

.PHONY : compiler-test
compiler-test: $(PYRET_TEST_PREREQ)
	$(NODE) $(PYRET_TEST_PHASE)/main-wrapper.js \
    --module-load-dir $(PYRET_TEST_PHASE)/arr/compiler/ \
    -check-all src/arr/compiler/compile.arr

.PHONY : lib-test
lib-test: $(PYRET_TEST_PREREQ)
	$(NODE) $(PYRET_TEST_PHASE)/main-wrapper.js \
    -check-all tests/lib-test/lib-test-main.arr

.PHONY : benchmark-test
benchmark-test: tools/benchmark/*.js $(PYRET_TEST_PREREQ)
	cd tools/benchmark && node tests

.PHONY : docs-test
docs-test: docs
	cd docs/written && scribble --htmls index.scrbl

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
release-gzip: $(PYRET_COMP) phase1 standalone1 $(RELEASE_DIR)/phase1
	gzip -c $(PHASE1)/pyret.js > $(RELEASE_DIR)/pyret.js
	(cd $(PHASE1) && find * -type d -print0) | parallel --gnu -0 mkdir -p '$(RELEASE_DIR)/phase1/{}'
	(cd $(PHASE1) && find * -type f -print0) | parallel --gnu -0 "gzip -c '$(PHASE1)/{}' > '$(RELEASE_DIR)/phase1/{}'"
horizon-gzip: standalone1 $(RELEASE_DIR)/phase1
	sed "s/define('pyret-start/define('pyret/" $(PHASE1)/pyret.js > $(RELEASE_DIR)/pyret-full.js
	gzip -c $(RELEASE_DIR)/pyret-full.js > $(RELEASE_DIR)/pyret.js
	(cd $(PHASE1) && find * -type d -print0) | parallel --gnu -0 mkdir -p '$(RELEASE_DIR)/phase1/{}'
	(cd $(PHASE1) && find * -type f -print0) | parallel --gnu -0 "gzip -c '$(PHASE1)/{}' > '$(RELEASE_DIR)/phase1/{}'"
# If you need information on using the s3 script, run `s3 --man'
horizon-release: horizon-gzip
	cd $(RELEASE_DIR) && \
	find * -type f -print0 | parallel --gnu -0 $(S3) add --header 'Content-Type:text/javascript' --header 'Content-Encoding:gzip' --acl 'public-read' ':pyret-horizon/current/{}' '{}'
release: release-gzip
	cd $(RELEASE_DIR) && \
	find * -type f -print0 | parallel --gnu -0 $(S3) add --header 'Content-Type:text/javascript' --header 'Content-Encoding:gzip' --acl 'public-read' ':pyret-releases/$(VERSION)/{}' '{}'
test-release: release-gzip
	cd $(RELEASE_DIR) && \
	find * -type f -print0 | parallel --gnu -0 $(S3) add --header 'Content-Type:text/javascript' --header 'Content-Encoding:gzip' --acl 'public-read' ':pyret-releases/$(VERSION)-test/{}' '{}'
horizon-docs: docs
	scp -r build/docs/ $(DOCS_TARGET)/horizon-$(VERSION)/
	chmod -R a+rx $(DOCS_TARGET)/horizon-$(VERSION)/
	cd $(DOCS_TARGET) && unlink horizon && ln -s horizon-$(VERSION) horizon
release-docs: docs
	scp -r build/docs/ $(DOCS_TARGET)/$(VERSION)/
	chmod -R a+rx $(DOCS_TARGET)/$(VERSION)/
	cd $(DOCS_TARGET) && unlink latest && ln -s $(VERSION) latest
else
release-gzip:
	$(error Cannot release from this platform)
release:
	$(error Cannot release from this platform)
test-release: release-gzip
	$(error Cannot release from this platform)
endif
