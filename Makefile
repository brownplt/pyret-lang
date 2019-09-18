.PHONY: all clean build parser web runtime

all: build parser

build: src/arr/compiler/pyret-parser.js
	pyret --checks none -c src/arr/compiler/pyret.arr -o build/phaseA/pyret.jarr

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

build/runtime/%.js : src/runtime/%.ts
	tsc $< --outDir $(RUNTIME_BUILD_DIR)

runtime-src-dir:
	mkdir -p $(RUNTIME_SRC_DIR)

STOPIFIED_BUILTINS := $(RUNTIME_JS_SRCS:$(RUNTIME_SRC_DIR)/%.js=$(RUNTIME_BUILD_DIR)/%.js.stopped) $(RUNTIME_TS_COMPILED_FILES:%.js=%.js.stopped)

%.js.stopped : %.js
	node src/webworker/scripts/stopify-compile.js $< $@

runtime-copy: build runtime-src-dir $(RUNTIME_TS_COMPILED_FILES)
	cp $(RUNTIME_JS_SRCS) $(RUNTIME_BUILD_DIR)
	cp $(RUNTIME_JSON_SRCS) $(RUNTIME_BUILD_DIR)
	cd src/runtime-arr/ && node ../../build/phaseA/pyret.jarr --build-runnable unified.arr --builtin-js-dir "$(shell pwd)/$(RUNTIME_BUILD_DIR)" --runtime-builtin-relative-path "./" --type-check true
	mv src/runtime-arr/compiled/project/* $(RUNTIME_BUILD_DIR)

runtime: runtime-copy $(STOPIFIED_BUILTINS)

web: build/worker/pyret-grammar.js src/arr/compiler/pyret-parser.js build/worker/bundled-node-compile-deps.js build/worker/page.html build/worker/main.js
	mkdir -p build/worker; 
	pyret --checks none --standalone-file "$(shell pwd)/src/webworker/worker-standalone.js" --deps-file "$(shell pwd)/build/worker/bundled-node-compile-deps.js" -c src/arr/compiler/webworker.arr -o build/worker/pyret.jarr

build/worker/runtime-files.json: src/webworker/scripts/runtime-bundler.ts runtime
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
