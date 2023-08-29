PYRET_COMP0      = build/phase0/pyret.jarr
CLOSURE          = java -jar deps/closure-compiler/compiler.jar
NODE             = node -max-old-space-size=8192
SWEETJS          = node_modules/sweet.js/bin/sjs --readable-names --module ./src/js/macros.js
JS               = js
JSBASE           = $(JS)/base
JSTROVE          = $(JS)/trove
BASE             = arr/base
TROVE            = arr/trove
COMPILER         = arr/compiler

PHASE0           = build/phase0
PHASEA           = build/phaseA
PHASEB           = build/phaseB
PHASEC           = build/phaseC
RELEASE_DIR      = build/release
# HACK HACK HACK (See https://github.com/npm/npm/issues/3738)
export PATH      := ./node_modules/.bin:../node_modules/.bin:../../node_modules/.bin:$(PATH)
SHELL := /bin/bash

# CUSTOMIZE THESE IF NECESSARY
PARSERS         := $(patsubst src/js/base/%-grammar.bnf,src/js/%-parser.js,$(wildcard src/$(JSBASE)/*-grammar.bnf))
COPY_JS          = $(patsubst src/js/base/%.js,src/js/%.js,$(wildcard src/$(JSBASE)/*.js)) \
	src/js/js-numbers.js
COPY_LIB         = $(wildcard lib/jglr/*.js)
COMPILER_FILES = $(wildcard src/arr/compiler/*.js) $(wildcard src/arr/compiler/*.arr) $(wildcard src/arr/compiler/locators/*.arr) $(wildcard src/js/trove/*.js) $(wildcard src/arr/trove/*.arr)
TROVE_ARR_FILES = $(wildcard src/arr/trove/*.arr)

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

PHASEA_ALL_DEPS := $(patsubst src/%,$(PHASEA)/%,$(COPY_JS)) $(patsubst lib/jglr/%.js,$(PHASEA)/js/%.js,$(COPY_LIB)) $(PHASEA)/bundled-node-compile-deps.js $(PHASEA)/bundled-node-deps.js $(PHASEA)/config.json
PHASEB_ALL_DEPS := $(patsubst src/%,$(PHASEB)/%,$(COPY_JS)) $(patsubst lib/jglr/%.js,$(PHASEB)/js/%.js,$(COPY_LIB)) $(PHASEB)/bundled-node-compile-deps.js $(PHASEB)/bundled-node-deps.js $(PHASEB)/config.json
PHASEC_ALL_DEPS := $(patsubst src/%,$(PHASEC)/%,$(COPY_JS)) $(patsubst lib/jglr/%.js,$(PHASEC)/js/%.js,$(COPY_LIB)) $(PHASEC)/bundled-node-compile-deps.js $(PHASEC)/bundled-node-deps.js $(PHASEC)/config.json

PHASEA_DIRS     := $(sort $(dir $(PHASEA_ALL_DEPS)))
PHASEB_DIRS     := $(sort $(dir $(PHASEB_ALL_DEPS)))
PHASEC_DIRS     := $(sort $(dir $(PHASEC_ALL_DEPS)))

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
.PHONY : phaseA
phaseA: $(PHASEA)/pyret.jarr

.PHONY : phaseA-deps
phaseA-deps: $(PYRET_COMPA) $(PHASEA_ALL_DEPS) $(COMPILER_FILES) $(patsubst src/%,$(PHASEA)/%,$(PARSERS))


$(PHASEA)/pyret.jarr: $(PYRET_COMPA) $(PHASEA_ALL_DEPS) $(COMPILER_FILES) $(patsubst src/%,$(PHASEA)/%,$(PARSERS))
	$(NODE) $(PYRET_COMP0) --outfile build/phaseA/pyret.jarr \
                      --build-runnable src/arr/compiler/pyret.arr \
                      --builtin-js-dir src/js/trove/ \
                      --builtin-arr-dir src/arr/trove/ \
                      --compiled-dir build/phaseA/compiled/ \
                      --deps-file build/phaseA/bundled-node-compile-deps.js \
                      -no-check-mode $(EF) \
                      --require-config src/scripts/standalone-configA.json

$(PHASEA)/libs.jarr: $(PHASEA)/pyret.jarr
	$(NODE) $(PHASEA)/pyret.jarr --outfile build/phaseA/base.jarr \
                      --build-runnable src/arr/compiler/libs.arr \
                      --builtin-js-dir src/js/trove/ \
                      --builtin-arr-dir src/arr/trove/ \
                      --compiled-dir build/phaseA/lib-compiled/ \
                      --deps-file build/phaseA/bundled-node-compile-deps.js \
                      -no-check-mode $(EF) \
                      --require-config src/scripts/standalone-configA.json

.PHONY : libA
libA : $(PHASEA)/libs.jarr

.PHONY : phaseB
phaseB: $(PHASEB)/pyret.jarr

$(PHASEB)/pyret.jarr: $(PHASEA)/pyret.jarr $(PHASEB_ALL_DEPS) $(patsubst src/%,$(PHASEB)/%,$(PARSERS))
	$(NODE) $(PHASEA)/pyret.jarr --outfile build/phaseB/pyret.jarr \
                      --build-runnable src/arr/compiler/pyret.arr \
                      --builtin-js-dir src/js/trove/ \
                      --builtin-arr-dir src/arr/trove/ \
                      --compiled-dir build/phaseB/compiled/ \
                      --deps-file build/phaseB/bundled-node-compile-deps.js \
                      -no-check-mode $(EF) \
                      --require-config build/phaseB/config.json


.PHONY : phaseC
phaseC: $(PHASEC)/pyret.jarr

$(PHASEC)/pyret.jarr: $(PHASEB)/pyret.jarr $(PHASEC_ALL_DEPS) $(patsubst src/%,$(PHASEC)/%,$(PARSERS))
	$(NODE) $(PHASEB)/pyret.jarr --outfile build/phaseC/pyret.jarr \
                      --build-runnable src/arr/compiler/pyret.arr \
                      --builtin-js-dir src/js/trove/ \
                      --builtin-arr-dir src/arr/trove/ \
                      --compiled-dir build/phaseC/compiled/ \
                      --deps-file build/phaseB/bundled-node-compile-deps.js \
                      -no-check-mode $(EF) \
                      --require-config build/phaseB/config.json

.PHONY : show-comp
show-comp: build/show-compilation.jarr

$(PHASEA)/bundled-node-compile-deps.js: src/js/trove/require-node-compile-dependencies.js
	browserify src/js/trove/require-node-compile-dependencies.js -o $@
$(PHASEA)/bundled-node-deps.js: src/js/trove/require-node-dependencies.js
	browserify src/js/trove/require-node-dependencies.js -o $@
$(PHASEB)/bundled-node-compile-deps.js: src/js/trove/require-node-compile-dependencies.js
	browserify src/js/trove/require-node-compile-dependencies.js -o $@
$(PHASEC)/bundled-node-compile-deps.js: src/js/trove/require-node-compile-dependencies.js
	browserify src/js/trove/require-node-compile-dependencies.js -o $@

$(PHASEA)/config.json: src/scripts/node_modules-config.json
	cp $< $@
$(PHASEB)/config.json: src/scripts/node_modules-config.json
	cp $< $@
$(PHASEC)/config.json: src/scripts/node_modules-config.json
	cp $< $@

showpath:
	@echo my new PATH = $(PATH)
	@echo `which browserify`

$(BUNDLED_DEPS): src/js/trove/require-node-dependencies.js
	npx browserify src/js/trove/require-node-dependencies.js -o $(BUNDLED_DEPS)

build/show-compilation.jarr: $(PHASEA)/pyret.jarr src/scripts/show-compilation.arr
	$(NODE) $(PHASEA)/pyret.jarr --outfile build/show-compilation.jarr \
                      --build-runnable src/scripts/show-compilation.arr \
                      --builtin-js-dir src/js/trove/ \
                      --builtin-arr-dir src/arr/trove/ \
                      --compiled-dir build/show-comp/compiled/ \
                      -no-check-mode \
                      --require-config src/scripts/standalone-configA.json
ifneq ($(EF),)
EXTRA_FLAGS=$(EF)
else
EXTRA_FLAGS = -no-check-mode
endif
%.jarr: $(PHASEA)/pyret.jarr %.arr
	$(NODE) $(PHASEA)/pyret.jarr --outfile $*.jarr \
                      --build-runnable $*.arr \
                      --builtin-js-dir src/js/trove/ \
                      --builtin-arr-dir src/arr/trove/ \
                      --compiled-dir compiled/ \
                      $(EXTRA_FLAGS) \
                      --require-config src/scripts/standalone-configA.json

%.html: $(PHASEA)/pyret.jarr %.arr
	$(NODE) $(PHASEA)/pyret.jarr --outfile $*.jarr \
                      --build-runnable $*.arr \
                      --builtin-js-dir src/js/trove/ \
                      --builtin-arr-dir src/arr/trove/ \
                      --compiled-dir compiled/ \
                      $(EXTRA_FLAGS) \
                      --require-config src/scripts/standalone-configA.json \
                      -bundle-dependencies \
                      --html-file $*.html

$(PHASEA_ALL_DEPS): | $(PHASEA)

$(PHASEB_ALL_DEPS): | $(PHASEB) phaseA

$(PHASEC_ALL_DEPS): | $(PHASEC) phaseB

$(PHASEA):
	@$(call MKDIR,$(PHASEA_DIRS))

$(PHASEB):
	@$(call MKDIR,$(PHASEB_DIRS))

$(PHASEC):
	@$(call MKDIR,$(PHASEC_DIRS))

$(PHASEA)/$(JS)/%-parser.js: src/$(JSBASE)/%-grammar.bnf src/$(JSBASE)/%-tokenizer.js $(wildcard lib/jglr/*.js)
	$(NODE) lib/jglr/parser-generator.js src/$(JSBASE)/$*-grammar.bnf $(PHASEA)/$(JS)/$*-grammar.js "../../../lib/jglr" "jglr/jglr" "pyret-base/js/$*-parser"
	$(NODE) $(PHASEA)/$(JS)/$*-grammar.js $(PHASEA)/$(JS)/$*-parser.js

$(PHASEB)/$(JS)/%-parser.js: src/$(JSBASE)/%-grammar.bnf src/$(JSBASE)/%-tokenizer.js $(wildcard lib/jglr/*.js)
	$(NODE) lib/jglr/parser-generator.js src/$(JSBASE)/$*-grammar.bnf $(PHASEB)/$(JS)/$*-grammar.js "../../../lib/jglr" "jglr/jglr" "pyret-base/js/$*-parser"
	$(NODE) $(PHASEB)/$(JS)/$*-grammar.js $(PHASEB)/$(JS)/$*-parser.js

$(PHASEC)/$(JS)/%-parser.js: src/$(JSBASE)/%-grammar.bnf src/$(JSBASE)/%-tokenizer.js $(wildcard lib/jglr/*.js)
	$(NODE) lib/jglr/parser-generator.js src/$(JSBASE)/$*-grammar.bnf $(PHASEC)/$(JS)/$*-grammar.js "../../../lib/jglr" "jglr/jglr" "pyret-base/js/$*-parser"
	$(NODE) $(PHASEC)/$(JS)/$*-grammar.js $(PHASEC)/$(JS)/$*-parser.js

$(PHASEA)/$(JS)/%.js : src/$(JSBASE)/%.js
	cp $< $@
$(PHASEA)/$(JS)/%.js : lib/jglr/%.js
	cp $< $@
$(PHASEB)/$(JS)/%.js : src/$(JSBASE)/%.js
	cp $< $@
$(PHASEB)/$(JS)/%.js : lib/jglr/%.js
	cp $< $@
$(PHASEC)/$(JS)/%.js : src/$(JSBASE)/%.js
	cp $< $@
$(PHASEC)/$(JS)/%.js : lib/jglr/%.js
	cp $< $@

.PHONY : install
install:
	@$(call MKDIR,node_modules)
	npm install

PYRET_TEST_PHASE=$(P)
ifeq ($(PYRET_TEST_PHASE),B)
  PYRET_TEST_PHASE=$(PHASEB)
  PYRET_TEST_PREREQ=$(PHASEB)/pyret.jarr
  PYRET_TEST_CONFIG=src/scripts/standalone-configB.json
else
ifeq ($(PYRET_TEST_PHASE),C)
  PYRET_TEST_PHASE=$(PHASEC)
  PYRET_TEST_PREREQ=$(PHASEC)/pyret.jarr
  PYRET_TEST_CONFIG=src/scripts/standalone-configC.json
else
  PYRET_TEST_PHASE=$(PHASEA)
  PYRET_TEST_PREREQ=$(PHASEA)/pyret.jarr
  PYRET_TEST_CONFIG=src/scripts/standalone-configA.json
endif
endif

TEST_BUILD=$(NODE) $(PYRET_TEST_PHASE)/pyret.jarr \
	  --builtin-js-dir src/js/trove/ \
		--builtin-arr-dir src/arr/trove/ \
		--require-config $(PYRET_TEST_CONFIG) \
		--compiled-dir tests/compiled/

.PHONY : test-all
test-all: test

.PHONY : test
test: pyret-test type-check-test pyret-io-test

.PHONY : parse-test
parse-test: tests/parse/parse.js build/phaseA/js/pyret-tokenizer.js build/phaseA/js/pyret-parser.js
	cd tests/parse/ && $(NODE) parse.js

TEST_FILES := $(wildcard tests/pyret/tests/*.arr)
TYPE_TEST_FILES := $(wildcard tests/type-check/bad/*.arr) $(wildcard tests/type-check/good/*.arr) $(wildcard tests/type-check/should/*.arr) $(wildcard tests/type-check/should-not/*.arr)
REG_TEST_FILES := $(wildcard tests/pyret/regression/*.arr)
MAIN_TEST_FILES := tests/pyret/main2.arr tests/type-check/main.arr tests/pyret/regression.arr tests/lib-test/lib-test-main.arr tests/all.arr

tests/pyret/all.jarr: phaseA $(TEST_FILES) $(TYPE_TEST_FILES) $(REG_TEST_FILES) $(MAIN_TEST_FILES)
	$(TEST_BUILD) \
		--build-runnable tests/all.arr \
    --outfile tests/pyret/all.jarr \
		-check-all

.PHONY : all-pyret-test
all-pyret-test: tests/pyret/all.jarr parse-test pyret-io-test
	$(NODE) tests/pyret/all.jarr

tests/pyret/main2.jarr: phaseA tests/pyret/main2.arr  $(TEST_FILES)
	$(TEST_BUILD) \
		--outfile tests/pyret/main2.jarr \
		--build-runnable tests/pyret/main2.arr \
		-check-all # NOTE(joe): check-all doesn't yet do anything


.PHONY : pyret-test
pyret-test: phaseA tests/pyret/main2.jarr
	$(NODE) tests/pyret/main2.jarr

.PHONY : pyret-io-test
pyret-io-test: phaseA
	npx jest --verbose "tests/io-tests/io.test.js"

.PHONY : regression-test
regression-test: tests/pyret/regression.jarr
	$(NODE) tests/pyret/regression.jarr

tests/pyret/regression.jarr: $(PYRET_TEST_PREREQ) $(REG_TEST_FILES) tests/pyret/regression.arr
	$(TEST_BUILD) \
		--build-runnable tests/pyret/regression.arr --outfile tests/pyret/regression.jarr

.PHONY : type-check-test
type-check-test: phaseA tests/type-check/main.jarr
	$(NODE) tests/type-check/main.jarr

tests/type-check/main.jarr: phaseA tests/type-check/main.arr $(TYPE_TEST_FILES)
	$(TEST_BUILD) \
		--build-runnable tests/type-check/main.arr --outfile tests/type-check/main.jarr


.PHONY : clean
clean:
	$(call RMDIR,$(PHASEA))
	$(call RMDIR,$(PHASEB))
	$(call RMDIR,$(PHASEC))
	$(call RMDIR,build/show-comp/compiled)
	$(call RMDIR,$(RELEASE_DIR))
	$(call RM,$(BUNDLED_DEPS))

.PHONY : test-clean
test-clean:
	$(call RMDIR, tests/compiled)

# Written this way because cmd.exe complains about && in command lines
new-bootstrap: no-diff-standalone $(PHASE0BUILD)
	cp $(PHASEC)/pyret.jarr $(PYRET_COMP0)
	cp -r $(PHASEC)/js $(PHASE0)/
no-diff-standalone: phaseB phaseC
	diff $(PHASEB)/pyret.jarr $(PHASEC)/pyret.jarr


