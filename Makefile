.PHONY: all clean build parser web runtime

all: build parser

PYRET_JARR_DEPS := $(wildcard src/arr/compiler/*.arr)

PYRET_JARR := build/phaseA/pyret.jarr

$(PYRET_JARR) : $(PYRET_JARR_DEPS)
	pyret --checks none -c src/arr/compiler/pyret.arr -o $(PYRET_JARR) 

BUILD_DEPS := \
	src/arr/compiler/pyret-parser.js \
	$(PYRET_JARR)

build: $(BUILD_DEPS)

all-tests: build runtime web
	jest --verbose

web-tests: web
	jest --verbose "browser"

stopify-web-tests: web
	jest --verbose "stopify"

offline-tests: build runtime
	jest --verbose "tests-new/simple-output.test.js"

WEBWORKER_BUILD_DIR := build/worker
WEBWORKER_SRC_DIR := src/webworker
RUNTIME_SRC_DIR := src/runtime
RUNTIME_BUILD_DIR := build/runtime
RUNTIME_JS_SRCS := $(wildcard $(RUNTIME_SRC_DIR)/*.js)
RUNTIME_JSON_SRCS := $(wildcard $(RUNTIME_SRC_DIR)/*.json)
RUNTIME_TS_SRCS := $(wildcard $(RUNTIME_SRC_DIR)/*.ts)
RUNTIME_TS_COMPILED_FILES := $(RUNTIME_TS_SRCS:$(RUNTIME_SRC_DIR)/%.ts=$(RUNTIME_BUILD_DIR)/%.js)
RUNTIME_COPIED_BUILTINS := \
	$(RUNTIME_JS_SRCS:$(RUNTIME_SRC_DIR)/%=$(RUNTIME_BUILD_DIR)/%) \
	$(RUNTIME_JSON_SRCS:$(RUNTIME_SRC_DIR)/%=$(RUNTIME_BUILD_DIR)/%)
RUNTIME_ARR_SRC_DIR := src/runtime-arr
RUNTIME_ARR_SRCS := $(wildcard $(RUNTIME_ARR_SRC_DIR)/*.arr)
RUNTIME_ARR_COMPILED_FILES := $(RUNTIME_ARR_SRCS:$(RUNTIME_ARR_SRC_DIR)/%.arr=$(RUNTIME_BUILD_DIR)/%.arr.js)
STOPIFIED_BUILTINS := \
	$(RUNTIME_JS_SRCS:$(RUNTIME_SRC_DIR)/%.js=$(RUNTIME_BUILD_DIR)/%.js.stopped) \
	$(RUNTIME_TS_SRCS:$(RUNTIME_SRC_DIR)/%.ts=$(RUNTIME_BUILD_DIR)/%.js.stopped) \
	$(RUNTIME_ARR_SRCS:$(RUNTIME_ARR_SRC_DIR)/%.arr=$(RUNTIME_BUILD_DIR)/%.arr.js.stopped)

$(RUNTIME_BUILD_DIR)/%.js : $(RUNTIME_SRC_DIR)/%.ts
	tsc $< --outDir $(RUNTIME_BUILD_DIR)

$(RUNTIME_SRC_DIR):
	mkdir -p $(RUNTIME_SRC_DIR)

$(RUNTIME_BUILD_DIR):
	mkdir -p $(RUNTIME_BUILD_DIR)

$(RUNTIME_BUILD_DIR)/%.js.stopped : $(RUNTIME_SRC_DIR)/%.js
	node src/webworker/scripts/stopify-compile.js $< $@

$(RUNTIME_BUILD_DIR)/%.js.stopped : $(RUNTIME_BUILD_DIR)/%.js
	node src/webworker/scripts/stopify-compile.js $< $@

$(RUNTIME_BUILD_DIR)/%.js : $(RUNTIME_SRC_DIR)/%.js
	cp $< $@

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
test: pyret-test type-check-test

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
all-pyret-test: tests/pyret/all.jarr parse-test
	$(NODE) tests/pyret/all.jarr

tests/pyret/main2.jarr: phaseA tests/pyret/main2.arr  $(TEST_FILES)
	$(TEST_BUILD) \
		--outfile tests/pyret/main2.jarr \
		--build-runnable tests/pyret/main2.arr \
		-check-all # NOTE(joe): check-all doesn't yet do anything


.PHONY : pyret-test
pyret-test: phaseA tests/pyret/main2.jarr
	$(NODE) tests/pyret/main2.jarr

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

$(RUNTIME_BUILD_DIR)/%.arr.js : $(RUNTIME_ARR_SRC_DIR)/%.arr
	cd $(RUNTIME_ARR_SRC_DIR) && node ../../build/phaseA/pyret.jarr \
		--build-runnable $*.arr \
		--builtin-js-dir "$(shell pwd)/$(RUNTIME_BUILD_DIR)" \
		--runtime-builtin-relative-path "./" \
		--type-check true
	mv $(RUNTIME_ARR_SRC_DIR)/compiled/project/$*.arr.js $(RUNTIME_BUILD_DIR)

RUNTIME_DEPS := \
	$(BUILD_DEPS) \
	$(RUNTIME_SRC_DIR) \
	$(RUNTIME_BUILD_DIR) \
	$(RUNTIME_TS_COMPILED_FILES) \
	$(RUNTIME_COPIED_BUILTINS) \
	$(RUNTIME_ARR_COMPILED_FILES) \
	$(STOPIFIED_BUILTINS) \

runtime: $(RUNTIME_DEPS)

WORKER_BUILD_DIR := build/worker

$(WORKER_BUILD_DIR):
	mkdir -p $(WORKER_BUILD_DIR)

build/worker/pyret.jarr : build/worker/pyret-grammar.js src/arr/compiler/pyret-parser.js build/worker/bundled-node-compile-deps.js $(WORKER_BUILD_DIR)
	pyret --checks none --standalone-file "$(shell pwd)/src/webworker/worker-standalone.js" --deps-file "$(shell pwd)/build/worker/bundled-node-compile-deps.js" -c src/arr/compiler/webworker.arr -o build/worker/pyret.jarr

web: build/worker/pyret.jarr build/worker/page.html build/worker/main.js

build/worker/runtime-files.json: src/webworker/scripts/runtime-bundler.ts $(RUNTIME_DEPS)
	tsc $(WEBWORKER_SRC_DIR)/scripts/runtime-bundler.ts --outDir $(WEBWORKER_BUILD_DIR)
	node $(WEBWORKER_BUILD_DIR)/runtime-bundler.js $(RUNTIME_BUILD_DIR) build/worker/runtime-files.json

build/worker/bundled-node-compile-deps.js: src/js/trove/require-node-compile-dependencies.js
	browserify src/js/trove/require-node-compile-dependencies.js -o $@

build/phaseA/pyret-grammar.js: lib/jglr/parser-generator.js 
	mkdir -p build/phaseA 
	mkdir -p build/worker 
	node lib/jglr/parser-generator.js src/js/base/pyret-grammar.bnf build/phaseA/pyret-grammar.js "../../lib/jglr" "jglr/jglr" "pyret-base/js/pyret-parser" 
 
src/arr/compiler/pyret-parser.js: build/phaseA/pyret-grammar.js 
	node build/phaseA/pyret-grammar.js src/arr/compiler/pyret-parser.js 
 
build/worker/pyret-grammar.js: build/phaseA/pyret-grammar.js 
	cp build/phaseA/pyret-grammar.js build/worker/pyret-grammar.js 
 
parser: src/arr/compiler/pyret-parser.js

build/worker/main.js: src/webworker/*.ts build/worker/runtime-files.json
	browserify $(WEBWORKER_SRC_DIR)/main.ts -p [ tsify ] -o $(WEBWORKER_BUILD_DIR)/main.js

build/worker/page.html: src/webworker/page.html
	cp $< $@

clean:
	rm -r -f build/phaseA build/worker
	rm -f src/arr/compiler/pyret-parser.js
	rm -r -f tests-new/.pyret
	rm -r -f build/runtime
