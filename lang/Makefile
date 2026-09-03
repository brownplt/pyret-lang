PYRET_COMP0      = build/phase0/pyret.jarr
NODE             = node -max-old-space-size=8192
NPM_EXEC         = npm --prefix . exec --no --
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
BUNDLED_DEPS     = build/bundled-node-deps.js
SHELL := /usr/bin/env bash

# CUSTOMIZE THESE IF NECESSARY
PARSERS         := $(patsubst src/js/base/%-grammar.bnf,src/js/%-parser.js,$(wildcard src/$(JSBASE)/*-grammar.bnf))
COPY_JS          = $(patsubst src/js/base/%.js,src/js/%.js,$(wildcard src/$(JSBASE)/*.js)) \
	src/js/js-numbers.js
COPY_LIB         = $(wildcard lib/jglr/*.js)
COMPILER_FILES = $(wildcard src/arr/compiler/*.js) $(wildcard src/arr/compiler/*.arr) $(wildcard src/arr/compiler/locators/*.arr) $(wildcard src/js/trove/*.js) $(wildcard src/arr/trove/*.arr)
TROVE_ARR_FILES = $(wildcard src/arr/trove/*.arr)
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

$(PHASEA)/base.jarr: $(PHASEA)/pyret.jarr
	$(NODE) $(PHASEA)/pyret.jarr --outfile build/phaseA/base.jarr \
                      --build-runnable src/arr/compiler/libs.arr \
                      --builtin-js-dir src/js/trove/ \
                      --builtin-arr-dir src/arr/trove/ \
                      --compiled-dir build/phaseA/lib-compiled/ \
                      --deps-file build/phaseA/bundled-node-compile-deps.js \
                      -no-check-mode $(EF) \
                      --require-config src/scripts/standalone-configA.json

.PHONY : libA
libA : $(PHASEA)/base.jarr

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
	$(NPM_EXEC) browserify src/js/trove/require-node-compile-dependencies.js -o $@
$(PHASEA)/bundled-node-deps.js: src/js/trove/require-node-dependencies.js
	$(NPM_EXEC) browserify src/js/trove/require-node-dependencies.js -o $@
$(PHASEB)/bundled-node-compile-deps.js: src/js/trove/require-node-compile-dependencies.js
	$(NPM_EXEC) browserify src/js/trove/require-node-compile-dependencies.js -o $@
$(PHASEC)/bundled-node-compile-deps.js: src/js/trove/require-node-compile-dependencies.js
	$(NPM_EXEC) browserify src/js/trove/require-node-compile-dependencies.js -o $@

$(PHASEA)/config.json: src/scripts/node_modules-config.json
	cp $< $@
$(PHASEB)/config.json: src/scripts/node_modules-config.json
	cp $< $@
$(PHASEC)/config.json: src/scripts/node_modules-config.json
	cp $< $@

$(BUNDLED_DEPS): src/js/trove/require-node-dependencies.js
	$(NPM_EXEC) browserify src/js/trove/require-node-dependencies.js -o $(BUNDLED_DEPS)

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

.PHONY : test
test: pyret-test compiler-test type-check-test pyret-io-test jsnums-test

.PHONY : parse-test
parse-test: tests/parse/parse.js build/phaseA/js/pyret-tokenizer.js build/phaseA/js/pyret-parser.js
	cd tests/parse/ && $(NODE) parse.js

TEST_FILES := $(wildcard tests/pyret/tests/*.arr)
TYPE_TEST_FILES := $(wildcard tests/type-check/bad/*.arr) $(wildcard tests/type-check/good/*.arr) $(wildcard tests/type-check/should/*.arr) $(wildcard tests/type-check/should-not/*.arr)
REG_TEST_FILES := $(wildcard tests/pyret/regression/*.arr)
MAIN_TEST_FILES := tests/pyret/main2.arr tests/pyret/compiler-tests.arr tests/type-check/main.arr tests/pyret/regression.arr tests/lib-test/lib-test-main.arr tests/all.arr

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


# main2 is the language/runtime suite: none of its modules import
# src/arr/compiler or compile anything at test runtime. The modules that DO
# live in compiler-tests.arr (see its header), whose run needs the
# tests/compiled cache warm -- on this .arr side, building the jarr into
# tests/compiled warms it as a side effect of the same $(TEST_BUILD).
.PHONY : pyret-test
pyret-test: phaseA tests/pyret/main2.jarr
	$(NODE) tests/pyret/main2.jarr

tests/pyret/compiler-tests.jarr: phaseA tests/pyret/compiler-tests.arr $(TEST_FILES)
	$(TEST_BUILD) \
		--outfile tests/pyret/compiler-tests.jarr \
		--build-runnable tests/pyret/compiler-tests.arr \
		-check-all

.PHONY : compiler-test
compiler-test: phaseA tests/pyret/compiler-tests.jarr
	$(NODE) tests/pyret/compiler-tests.jarr

.PHONY : pyret-io-test
pyret-io-test: phaseA
	$(NPM_EXEC) jest --detectOpenHandles --forceExit --verbose "tests/io-tests/io.test.js"

.PHONY : jsnums-test
jsnums-test: phaseA
	$(NODE) tests/jsnums-test/jsnums-test.js

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
	-git clean -fdX tests/scratch

# Written this way because cmd.exe complains about && in command lines
new-bootstrap: no-diff-standalone $(PHASE0BUILD)
	cp $(PHASEC)/pyret.jarr $(PYRET_COMP0)
	cp -r $(PHASEC)/js $(PHASE0)/
no-diff-standalone: phaseB phaseC
	diff $(PHASEB)/pyret.jarr $(PHASEC)/pyret.jarr

# ============================================================
# TypeScript compiler port (strictly additive; see src/ts-compiler/)
# Builds a TS implementation of the arr/compiler pipeline that
# accepts the same CLI options and emits the same module format.
# ============================================================

TS_COMP_DIR      = src/ts-compiler
TS_COMP_OUT      = build/ts-compiler
TS_PYRET         = $(NODE) $(TS_COMP_OUT)/pyret.js
TS_COMP_SRC      = $(wildcard $(TS_COMP_DIR)/src/*.ts) $(wildcard $(TS_COMP_DIR)/src/*/*.ts)

$(TS_COMP_OUT)/js/pyret-parser.js: src/$(JSBASE)/pyret-grammar.bnf src/$(JSBASE)/pyret-tokenizer.js $(wildcard lib/jglr/*.js)
	@$(call MKDIR,$(TS_COMP_OUT)/js)
	$(NODE) lib/jglr/parser-generator.js src/$(JSBASE)/pyret-grammar.bnf $(TS_COMP_OUT)/js/pyret-grammar.js "../../../lib/jglr" "jglr/jglr" "pyret-base/js/pyret-parser"
	$(NODE) $(TS_COMP_OUT)/js/pyret-grammar.js $(TS_COMP_OUT)/js/pyret-parser.js

$(TS_COMP_DIR)/node_modules/.bin/tsc:
	npm --prefix $(TS_COMP_DIR) install --no-audit --no-fund

$(TS_COMP_OUT)/pyret.js: $(TS_COMP_SRC) $(TS_COMP_DIR)/tsconfig.json $(TS_COMP_DIR)/node_modules/.bin/tsc $(TS_COMP_OUT)/js/pyret-parser.js
	$(TS_COMP_DIR)/node_modules/.bin/tsc -p $(TS_COMP_DIR)


# Runtime/support files for the TS compiler's output dir (mirrors PHASEA_ALL_DEPS)
TS_COMP_ALL_DEPS := $(patsubst src/%,$(TS_COMP_OUT)/%,$(COPY_JS)) $(patsubst lib/jglr/%.js,$(TS_COMP_OUT)/js/%.js,$(COPY_LIB)) $(TS_COMP_OUT)/bundled-node-compile-deps.js $(TS_COMP_OUT)/bundled-node-deps.js $(TS_COMP_OUT)/config.json

$(TS_COMP_OUT)/js/%.js: src/js/base/%.js
	@$(call MKDIR,$(TS_COMP_OUT)/js)
	cp $< $@

$(TS_COMP_OUT)/js/%.js: lib/jglr/%.js
	@$(call MKDIR,$(TS_COMP_OUT)/js)
	cp $< $@

$(TS_COMP_OUT)/bundled-node-compile-deps.js: src/js/trove/require-node-compile-dependencies.js
	$(NPM_EXEC) browserify src/js/trove/require-node-compile-dependencies.js -o $@

$(TS_COMP_OUT)/bundled-node-deps.js: src/js/trove/require-node-dependencies.js
	$(NPM_EXEC) browserify src/js/trove/require-node-dependencies.js -o $@

$(TS_COMP_OUT)/config.json: src/scripts/node_modules-config.json
	cp $< $@

.PHONY : ts-compiler
ts-compiler: $(TS_COMP_OUT)/pyret.js $(TS_COMP_ALL_DEPS)

# Precompiled builtins for the TS compiler's --serve mode, mirroring libA:
# the serve handler resolves lib-compiled/ next to its own pyret.js (see
# src/ts-compiler/src/server.ts), the way the Pyret-hosted server uses
# build/phaseA/lib-compiled. Used by the npm CLI's `--backend ts`.
$(TS_COMP_OUT)/libs.jarr: $(TS_COMP_OUT)/pyret.js $(TS_COMP_ALL_DEPS) $(CONFIGA_RUNTIME_FILES)
	$(TS_PYRET) --outfile $(TS_COMP_OUT)/libs.jarr \
                      --build-runnable src/arr/compiler/libs.arr \
                      --builtin-js-dir src/js/trove/ \
                      --builtin-arr-dir src/arr/trove/ \
                      --compiled-dir $(TS_COMP_OUT)/lib-compiled/ \
                      --deps-file $(TS_COMP_OUT)/bundled-node-compile-deps.js \
                      -no-check-mode $(EF) \
                      --require-config src/scripts/standalone-configA.json

.PHONY : ts-libA
ts-libA : $(TS_COMP_OUT)/libs.jarr

# Unit tests for the TS compiler implementation itself
.PHONY : ts-unit-test
ts-unit-test: ts-compiler
	$(NODE) $(TS_COMP_DIR)/tests/run-unit-tests.js

# Parity: compile and run the same programs with the Pyret-hosted
# compiler (phaseA) and the TS compiler, with the same options, and
# compare results.
.PHONY : ts-parity-test
ts-parity-test: ts-compiler $(PHASEA)/pyret.jarr
	bash $(TS_COMP_DIR)/tests/parity-test.sh

# Type-checker parity: the whole tests/type-check corpus, compiled with
# -type-check by both compilers, requiring identical diagnostics. This is
# the coverage for type-check.ts itself -- the in-suite type-check tests
# (main.arr) import src/arr/compiler/* and so only exercise the .arr type
# checker, whichever compiler built them. See the header of the script.
.PHONY : ts-type-check-parity
ts-type-check-parity: ts-compiler $(PHASEA)/pyret.jarr
	bash $(TS_COMP_DIR)/tests/type-check-parity.sh

# Well-formedness/scope-error parity: the inline programs from the in-suite
# wf and compile-error tests (extracted at runtime, so the corpus tracks the
# suite), compiled by both compilers with default options, identical
# diagnostics required. Direct coverage for well-formed.ts/resolve-scope.ts
# error rendering at the CLI -- the in-suite tests only cover the .arr
# checker, whichever compiler built them. See the script header.
.PHONY : ts-wf-parity
ts-wf-parity: ts-compiler $(PHASEA)/pyret.jarr
	bash $(TS_COMP_DIR)/tests/wf-parity.sh

# The npm CLI's compile-server path (`pyret --backend ts`): starts pyret.js
# in -serve mode exactly as the npm client does and drives the ws+unix
# protocol -- good compile (and the standalone RUNS), failing compile,
# reconnect, shutdown. Needs ts-libA because the serve handler injects
# lib-compiled as a read-only cache (how the npm package ships builtins).
.PHONY : ts-serve-test
ts-serve-test: ts-compiler $(TS_COMP_OUT)/libs.jarr $(CONFIGA_RUNTIME_FILES)
	bash $(TS_COMP_DIR)/tests/serve-test.sh

# standalone-configA.json is shared with the .arr side and names
# build/phaseA/js/* for the runtime support files (baseUrl build/phaseA),
# so everything built with it needs those present -- even when the driving
# compiler is the TS one and build/phaseA/pyret.jarr is never built. These
# are plain copies plus the generated parser (i.e. what phaseA-deps builds),
# NOT a compiler bootstrap. Declared so a clean tree can go straight to
# `make ts-test` / `make all-ts-pyret-test` without building phaseA first.
CONFIGA_RUNTIME_FILES := $(PHASEA_ALL_DEPS) $(patsubst src/%,$(PHASEA)/%,$(PARSERS))

# Build and run the main Pyret test suite using the TS compiler
TS_TEST_BUILD=$(TS_PYRET) \
	  --builtin-js-dir src/js/trove/ \
		--builtin-arr-dir src/arr/trove/ \
		--require-config src/scripts/standalone-configA.json \
		--compiled-dir tests/ts-compiled/

tests/pyret/main2-ts.jarr: ts-compiler $(CONFIGA_RUNTIME_FILES) tests/pyret/main2.arr $(TEST_FILES)
	$(TS_TEST_BUILD) \
		--outfile tests/pyret/main2-ts.jarr \
		--build-runnable tests/pyret/main2.arr \
		-check-all

# Warming a compile cache: compile a trivial program into the target dir,
# which drags in every builtin. Without a warm cache, each in-suite compile
# (every repl-test restart) recompiles all builtins -- hours instead of
# seconds. Compiled caches are interchangeable between the two compilers,
# so the TS compiler can warm either dir. Stamp file is <dir>/.ts-suite-warm.
#
# The scratch program is deleted once the cache is populated, and is NOT its
# own make target (a target would go missing -> rebuild -> rewarm every time).
# It has to go: tests/pyret/tests/test-pprint.arr walks the whole lang/ tree at
# runtime and round-trips every .arr it finds at 3 widths, so any .arr left
# under lang/ -- build artifacts included -- silently adds 3 to the suite's
# test count and makes the TS total differ from the .arr total. Per-dir names
# keep parallel warms from racing; only the .jarr (not matched by the walk)
# is left behind.
tests/%/.ts-suite-warm: $(TS_COMP_OUT)/pyret.js $(TS_COMP_ALL_DEPS) $(CONFIGA_RUNTIME_FILES)
	@$(call MKDIR,tests/$*)
	echo '"warm"' > $(TS_COMP_OUT)/suite-cache-warm-$*.arr
	$(TS_PYRET) --build-runnable $(TS_COMP_OUT)/suite-cache-warm-$*.arr \
                      --outfile $(TS_COMP_OUT)/suite-cache-warm-$*.jarr \
                      --builtin-js-dir src/js/trove/ \
                      --builtin-arr-dir src/arr/trove/ \
                      --compiled-dir tests/$*/ \
                      --deps-file $(TS_COMP_OUT)/bundled-node-compile-deps.js \
                      -no-check-mode -no-display-progress \
                      --require-config src/scripts/standalone-configA.json
	$(call RM,$(TS_COMP_OUT)/suite-cache-warm-$*.arr)
	touch $@

# The suite's own compiler tests (test-repl.arr and friends) compile through
# CLI.default-test-context, whose module cache is tests/compiled/. The stock
# suite warms that cache as a side effect of building main2.jarr there; the ts
# build keeps its own compile cache in tests/ts-compiled/, so tests/compiled/
# has to be warmed explicitly.
TS_SUITE_CACHE_WARM=tests/compiled/.ts-suite-warm
# repl-test drives the in-process load-lib runtime, whose cacheBaseDir is
# tests/ts-compiled (src/ts-compiler/tests/in-process-host.js).
TS_COMPILED_CACHE_WARM=tests/ts-compiled/.ts-suite-warm
# Both stamps are already named as prerequisites below, so make won't treat
# them as intermediates -- but say so, since a deleted stamp means an
# unnecessary rewarm.
.SECONDARY: $(TS_SUITE_CACHE_WARM) $(TS_COMPILED_CACHE_WARM)

# main2 no longer contains compile-at-runtime tests (they moved to
# compiler-tests.arr), so the TS-built run needs NO cache warm: this is a
# pure TS-codegen suite.
.PHONY : ts-pyret-test
ts-pyret-test: tests/pyret/main2-ts.jarr
	$(NODE) tests/pyret/main2-ts.jarr

# There is deliberately NO ts-compiler-test: the TS compiler's build of
# compiler-tests.arr is byte-identical to phaseA's (verified 2026-08-10,
# clean-room: fresh compiled dirs, same worklist, sha256-equal 35MB jarrs),
# so a TS-built run would re-run the same bytes `make compiler-test` runs.
# The TS compiler still compiles these modules as part of all-ts.jarr, which
# keeps them as codegen inputs; to narrow an all-ts failure in one of them,
# use `make compiler-test` -- it is the same program.

# Mirrors all-pyret-test on the .arr side: one build, one run, covering
# main2 + type-check + regression + lib-test. The narrower ts-pyret-test /
# ts-type-check-test / ts-regression-test targets remain available for
# narrowing down a failure by hand, exactly as their .arr counterparts do.
.PHONY : ts-test
ts-test: ts-unit-test ts-parity-test ts-type-check-parity ts-wf-parity ts-repl-test ts-serve-test all-ts-pyret-test ts-io-test

.PHONY : ts-clean
ts-clean:
	$(call RMDIR,$(TS_COMP_OUT))
	$(call RMDIR,$(PHASEBTS))
	$(call RMDIR,$(PHASECTS))
	$(call RMDIR,tests/ts-compiled)
	$(call RMDIR,compiled-ts)
	$(call RMDIR,build/serve-smoke-compiled)
	$(call RM,$(TS_SUITE_CACHE_WARM))
	$(call RM,tests/pyret/main2-ts.jarr)
	$(call RM,tests/pyret/regression-ts.jarr)
	$(call RM,tests/type-check/main-ts.jarr)
	$(call RM,tests/pyret/all-ts.jarr)

# Analogous to the %.jarr rule above, but using the TypeScript compiler:
# `make path/to/foo.ts.jarr` builds path/to/foo.arr into path/to/foo.ts.jarr.
# Same flag override applies (EF="..." to replace -no-check-mode).
%.ts.jarr: $(TS_COMP_OUT)/pyret.js %.arr
	$(TS_PYRET) --outfile $*.ts.jarr \
                      --build-runnable $*.arr \
                      --builtin-js-dir src/js/trove/ \
                      --builtin-arr-dir src/arr/trove/ \
                      --compiled-dir compiled-ts/ \
                      $(EXTRA_FLAGS) \
                      --require-config src/scripts/standalone-configA.json

# TS-compiler analogues of the remaining test entry points
tests/pyret/regression-ts.jarr: ts-compiler $(CONFIGA_RUNTIME_FILES) tests/pyret/regression.arr $(REG_TEST_FILES)
	$(TS_TEST_BUILD) \
		--build-runnable tests/pyret/regression.arr --outfile tests/pyret/regression-ts.jarr

.PHONY : ts-regression-test
ts-regression-test: tests/pyret/regression-ts.jarr
	$(NODE) tests/pyret/regression-ts.jarr

tests/type-check/main-ts.jarr: ts-compiler $(CONFIGA_RUNTIME_FILES) tests/type-check/main.arr $(TYPE_TEST_FILES)
	$(TS_TEST_BUILD) \
		--build-runnable tests/type-check/main.arr --outfile tests/type-check/main-ts.jarr

.PHONY : ts-type-check-test
ts-type-check-test: ts-compiler tests/type-check/main-ts.jarr
	$(NODE) tests/type-check/main-ts.jarr

.PHONY : ts-io-test
ts-io-test: ts-compiler
	$(NPM_EXEC) jest --detectOpenHandles --forceExit --verbose "src/ts-compiler/tests/io-ts.test.js"

tests/pyret/all-ts.jarr: ts-compiler $(CONFIGA_RUNTIME_FILES) $(TEST_FILES) $(TYPE_TEST_FILES) $(REG_TEST_FILES) $(MAIN_TEST_FILES)
	$(TS_TEST_BUILD) \
		--build-runnable tests/all.arr \
    --outfile tests/pyret/all-ts.jarr \
		-check-all

.PHONY : all-ts-pyret-test
all-ts-pyret-test: tests/pyret/all-ts.jarr $(TS_SUITE_CACHE_WARM)
	$(NODE) tests/pyret/all-ts.jarr

.PHONY : ts-repl-test
# The repl test drives repl.ts against a REAL in-process load-lib runtime
# (src/ts-compiler/tests/in-process-host.js), so it needs a warm builtin cache
# in tests/ts-compiled/ -- the cheap warm above, not a full main2-ts.jarr
# build, since the test only exercises base builtins. Uses the node:test
# runner because the in-process runtime pulls in requirejs, which can't load
# inside a jest worker.
ts-repl-test: ts-compiler $(TS_COMPILED_CACHE_WARM)
	$(NODE) --test src/ts-compiler/tests/repl-test.js

.PHONY : ts-stack-safety-test
# Compiles generated flat-but-large programs in subprocesses pinned to a
# browser-like --stack-size; `todo` cases are the known-overflow frontier.
ts-stack-safety-test: ts-compiler
	$(NODE) --test src/ts-compiler/tests/stack-safety-test.js

# ============================================================
# Bootstrap convergence: byte-identical, self-fixpointing compilers
# from BOTH the Pyret-hosted (.arr) path and the TypeScript path.
#
# The existing .arr chain is phase0 -> phaseA -> phaseB -> phaseC, with
# `no-diff-standalone` asserting phaseB == phaseC (the .arr self-fixpoint).
#
# Here we add a TS-seeded chain that mirrors phaseB/phaseC exactly but
# swaps the driving compiler for build/ts-compiler/pyret.js:
#   phaseB-ts = ts-compiler compiles src/arr/compiler/pyret.arr
#   phaseC-ts = phaseB-ts    compiles src/arr/compiler/pyret.arr
# Each stage uses the SAME CLI options as its .arr counterpart (same
# --require-config content, same --deps-file content, same flags); only
# the output/compiled dirs and the driving compiler differ.  Because
# $PYRET (this-pyret-dir) resolves to the *running* compiler's own dir
# and only the resolved file *contents* are bundled -- identical across
# all phase dirs -- any byte difference is a real codegen difference, not
# path noise.  (The existing phaseB-vs-phaseC diff already relies on this
# same path-independence.)
# ============================================================

PHASEBTS         = build/phaseB-ts
PHASECTS         = build/phaseC-ts

PHASEBTS_ALL_DEPS := $(patsubst src/%,$(PHASEBTS)/%,$(COPY_JS)) $(patsubst lib/jglr/%.js,$(PHASEBTS)/js/%.js,$(COPY_LIB)) $(PHASEBTS)/bundled-node-compile-deps.js $(PHASEBTS)/bundled-node-deps.js $(PHASEBTS)/config.json
PHASECTS_ALL_DEPS := $(patsubst src/%,$(PHASECTS)/%,$(COPY_JS)) $(patsubst lib/jglr/%.js,$(PHASECTS)/js/%.js,$(COPY_LIB)) $(PHASECTS)/bundled-node-compile-deps.js $(PHASECTS)/bundled-node-deps.js $(PHASECTS)/config.json

PHASEBTS_DIRS    := $(sort $(dir $(PHASEBTS_ALL_DEPS)))
PHASECTS_DIRS    := $(sort $(dir $(PHASECTS_ALL_DEPS)))

.PHONY : phaseB-ts
phaseB-ts: $(PHASEBTS)/pyret.jarr

# phaseB-ts: the TS compiler compiles the compiler source.  Mirrors the
# phaseB rule (own build/phaseB-ts/{config,deps}); driver is $(TS_PYRET).
$(PHASEBTS)/pyret.jarr: $(TS_COMP_OUT)/pyret.js $(TS_COMP_ALL_DEPS) $(PHASEBTS_ALL_DEPS) $(patsubst src/%,$(PHASEBTS)/%,$(PARSERS))
	$(TS_PYRET) --outfile $(PHASEBTS)/pyret.jarr \
                      --build-runnable src/arr/compiler/pyret.arr \
                      --builtin-js-dir src/js/trove/ \
                      --builtin-arr-dir src/arr/trove/ \
                      --compiled-dir $(PHASEBTS)/compiled/ \
                      --deps-file $(PHASEBTS)/bundled-node-compile-deps.js \
                      -no-check-mode $(EF) \
                      --require-config $(PHASEBTS)/config.json

.PHONY : phaseC-ts
phaseC-ts: $(PHASECTS)/pyret.jarr

# phaseC-ts: phaseB-ts compiles the compiler source (TS-seeded self-build).
# Mirrors the phaseC rule (reuses phaseB-ts's config/deps); driver is the
# freshly built phaseB-ts standalone.
$(PHASECTS)/pyret.jarr: $(PHASEBTS)/pyret.jarr $(PHASECTS_ALL_DEPS) $(patsubst src/%,$(PHASECTS)/%,$(PARSERS))
	$(NODE) $(PHASEBTS)/pyret.jarr --outfile $(PHASECTS)/pyret.jarr \
                      --build-runnable src/arr/compiler/pyret.arr \
                      --builtin-js-dir src/js/trove/ \
                      --builtin-arr-dir src/arr/trove/ \
                      --compiled-dir $(PHASECTS)/compiled/ \
                      --deps-file $(PHASEBTS)/bundled-node-compile-deps.js \
                      -no-check-mode $(EF) \
                      --require-config $(PHASEBTS)/config.json

# --- Support-file plumbing for the two TS-seeded phase dirs (mirrors the
# --- phaseA/B/C plumbing near the top of this Makefile) ---
$(PHASEBTS_ALL_DEPS): | $(PHASEBTS)
$(PHASECTS_ALL_DEPS): | $(PHASECTS) phaseB-ts

$(PHASEBTS):
	@$(call MKDIR,$(PHASEBTS_DIRS))
$(PHASECTS):
	@$(call MKDIR,$(PHASECTS_DIRS))

$(PHASEBTS)/$(JS)/%-parser.js: src/$(JSBASE)/%-grammar.bnf src/$(JSBASE)/%-tokenizer.js $(wildcard lib/jglr/*.js)
	$(NODE) lib/jglr/parser-generator.js src/$(JSBASE)/$*-grammar.bnf $(PHASEBTS)/$(JS)/$*-grammar.js "../../../lib/jglr" "jglr/jglr" "pyret-base/js/$*-parser"
	$(NODE) $(PHASEBTS)/$(JS)/$*-grammar.js $(PHASEBTS)/$(JS)/$*-parser.js
$(PHASECTS)/$(JS)/%-parser.js: src/$(JSBASE)/%-grammar.bnf src/$(JSBASE)/%-tokenizer.js $(wildcard lib/jglr/*.js)
	$(NODE) lib/jglr/parser-generator.js src/$(JSBASE)/$*-grammar.bnf $(PHASECTS)/$(JS)/$*-grammar.js "../../../lib/jglr" "jglr/jglr" "pyret-base/js/$*-parser"
	$(NODE) $(PHASECTS)/$(JS)/$*-grammar.js $(PHASECTS)/$(JS)/$*-parser.js

$(PHASEBTS)/$(JS)/%.js : src/$(JSBASE)/%.js
	cp $< $@
$(PHASEBTS)/$(JS)/%.js : lib/jglr/%.js
	cp $< $@
$(PHASECTS)/$(JS)/%.js : src/$(JSBASE)/%.js
	cp $< $@
$(PHASECTS)/$(JS)/%.js : lib/jglr/%.js
	cp $< $@

$(PHASEBTS)/bundled-node-compile-deps.js: src/js/trove/require-node-compile-dependencies.js
	$(NPM_EXEC) browserify src/js/trove/require-node-compile-dependencies.js -o $@
$(PHASEBTS)/bundled-node-deps.js: src/js/trove/require-node-dependencies.js
	$(NPM_EXEC) browserify src/js/trove/require-node-dependencies.js -o $@
$(PHASECTS)/bundled-node-compile-deps.js: src/js/trove/require-node-compile-dependencies.js
	$(NPM_EXEC) browserify src/js/trove/require-node-compile-dependencies.js -o $@
$(PHASECTS)/bundled-node-deps.js: src/js/trove/require-node-dependencies.js
	$(NPM_EXEC) browserify src/js/trove/require-node-dependencies.js -o $@

$(PHASEBTS)/config.json: src/scripts/node_modules-config.json
	cp $< $@
$(PHASECTS)/config.json: src/scripts/node_modules-config.json
	cp $< $@

.PHONY : no-diff-ts
no-diff-ts:
	rm -rf $(PHASEBTS)/pyret.jarr $(PHASEBTS)/compiled $(PHASECTS)/pyret.jarr $(PHASECTS)/compiled
	$(MAKE) phaseB-ts phaseC-ts
	diff $(PHASEBTS)/pyret.jarr $(PHASECTS)/pyret.jarr

# The grand convergence check.  Builds both chains and asserts that all
# four standalones are one byte-identical fixpoint reached from two
# independent seeds (the committed phase0 chain and the TS compiler).
#   * phaseB    == phaseC     (.arr self-fixpoint; == existing no-diff-standalone)
#   * phaseB-ts == phaseC-ts  (TS  self-fixpoint)
#   * phaseB    == phaseB-ts  (headline: TS compiler is a byte-exact drop-in)
# Prints sha256 of all four and a PASS/FAIL line per invariant; exits
# nonzero if any invariant fails.
.PHONY : bootstrap-converge
bootstrap-converge: phaseB phaseC phaseB-ts phaseC-ts
	@B=$(PHASEB)/pyret.jarr; C=$(PHASEC)/pyret.jarr; \
	 BT=$(PHASEBTS)/pyret.jarr; CT=$(PHASECTS)/pyret.jarr; \
	 echo "=== bootstrap convergence ==="; \
	 sha256sum $$B $$C $$BT $$CT; \
	 echo "---"; \
	 fail=0; \
	 if cmp -s $$B  $$C;  then echo "PASS  .arr fixpoint       phaseB    == phaseC";    else echo "FAIL  .arr fixpoint       phaseB    != phaseC";    fail=1; fi; \
	 if cmp -s $$BT $$CT; then echo "PASS  ts   fixpoint       phaseB-ts == phaseC-ts"; else echo "FAIL  ts   fixpoint       phaseB-ts != phaseC-ts"; fail=1; fi; \
	 if cmp -s $$B  $$BT; then echo "PASS  cross (headline)    phaseB    == phaseB-ts"; else echo "FAIL  cross (headline)    phaseB    != phaseB-ts"; fail=1; fi; \
	 echo "---"; \
	 if [ $$fail -eq 0 ]; then echo "CONVERGED: all four standalones are byte-identical"; \
	 else echo "NOT CONVERGED: see FAILs above (diff the named .jarr files to diagnose)"; fi; \
	 exit $$fail

# ============================================================
# Pause schedules: run the main2 suite with varied initial GAS/RUNGAS
# (see tests/pause-schedules/). Each scheduled run's output must match
# the unscheduled baseline run, and the rand-small build is repeated
# with the TS compiler (same compiled cache) and must be byte-identical
# to the .arr build.
# ============================================================

PAUSE_SCHEDULES := const-small const-medium rand-small rand-wide alternate sawtooth

tests/pyret/main2-ps-%.jarr: phaseA tests/pyret/main2.arr $(TEST_FILES) tests/pause-schedules/%.js
	$(TEST_BUILD) \
		--outfile $@ \
		--build-runnable tests/pyret/main2.arr \
		--pause-schedule tests/pause-schedules/$*.js \
		-check-all

# Shares tests/compiled with the .arr builds: cached module code depends on
# the worklist that warmed the cache (gensym numbering), so a byte-equal
# standalone needs both compilers reading the same cache. Module-codegen
# parity itself is covered by ts-parity-test / bootstrap-converge.
tests/pyret/main2-ps-ts-rand-small.jarr: ts-compiler $(CONFIGA_RUNTIME_FILES) tests/pyret/main2-ps-rand-small.jarr tests/pyret/main2.arr $(TEST_FILES) tests/pause-schedules/rand-small.js
	$(TS_PYRET) \
		--builtin-js-dir src/js/trove/ \
		--builtin-arr-dir src/arr/trove/ \
		--require-config src/scripts/standalone-configA.json \
		--compiled-dir tests/compiled/ \
		--outfile $@ \
		--build-runnable tests/pyret/main2.arr \
		--pause-schedule tests/pause-schedules/rand-small.js \
		-check-all

.PHONY : pause-schedule-test
pause-schedule-test: phaseA tests/pyret/main2.jarr $(patsubst %,tests/pyret/main2-ps-%.jarr,$(PAUSE_SCHEDULES)) tests/pyret/main2-ps-ts-rand-small.jarr
	cmp tests/pyret/main2-ps-rand-small.jarr tests/pyret/main2-ps-ts-rand-small.jarr
	$(NODE) tests/pyret/main2.jarr > build/pause-schedule-baseline.out
	set -e; for s in $(PAUSE_SCHEDULES); do \
	  echo "== pause schedule: $$s"; \
	  $(NODE) tests/pyret/main2-ps-$$s.jarr > build/pause-schedule-$$s.out; \
	  diff build/pause-schedule-baseline.out build/pause-schedule-$$s.out; \
	  echo "ok $$s"; \
	done

.PHONY : pause-schedule-clean
pause-schedule-clean:
	$(call RM,tests/pyret/main2-ps-const-small.jarr)
	$(call RM,tests/pyret/main2-ps-const-medium.jarr)
	$(call RM,tests/pyret/main2-ps-rand-small.jarr)
	$(call RM,tests/pyret/main2-ps-rand-wide.jarr)
	$(call RM,tests/pyret/main2-ps-alternate.jarr)
	$(call RM,tests/pyret/main2-ps-sawtooth.jarr)
	$(call RM,tests/pyret/main2-ps-ts-rand-small.jarr)
