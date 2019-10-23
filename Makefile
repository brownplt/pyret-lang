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

$(RUNTIME_BUILD_DIR)/%.json : $(RUNTIME_SRC_DIR)/%.json
	cp $< $@

$(RUNTIME_BUILD_DIR)/%.arr.js : $(RUNTIME_ARR_SRC_DIR)/%.arr
	cd $(RUNTIME_ARR_SRC_DIR) && node ../../build/phaseA/pyret.jarr \
		--build-runnable $*.arr \
		--builtin-js-dir "$(shell pwd)/$(RUNTIME_BUILD_DIR)" \
		--runtime-builtin-relative-path "./" \
		--type-check true
	mv $(RUNTIME_ARR_SRC_DIR)/compiled/project/$*.arr.js $(RUNTIME_BUILD_DIR)
	mv $(RUNTIME_ARR_SRC_DIR)/compiled/project/$*.arr.json $(RUNTIME_BUILD_DIR)

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
