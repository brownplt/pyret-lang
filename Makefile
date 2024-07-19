.PHONY: all clean build parser web runtime fix-runtime offline-tests clean-tests ide-tests ide

all: build parser runtime

PYRET_JARR_DEPS := $(wildcard src/arr/compiler/*.arr) $(patsubst src/arr/compiler/%.ts,src/arr/compiler/%.js,$(wildcard src/arr/compiler/*.ts))

PYRET_JARR := build/phaseA/pyret.jarr

$(PYRET_JARR) : $(PYRET_JARR_DEPS)
	npx pyret --checks none -c src/arr/compiler/pyret.arr -o $(PYRET_JARR)

src/arr/compiler/%.js : src/arr/compiler/%.ts
	npx tsc --project tsconfig.compiler.json
# Thanks internet! https://unix.stackexchange.com/a/65691
# This solves the problem that tsc (rightfully) inserts a semicolon at the end
# of the expression-statement in JS, but that can't be interpreted correctly
# when the JS module's text is put in expression position in the standalone.
# So chop the trailing ;
# UNFORTUNATELY, we can't do this while compiling individual .ts files,
# because tsc might overwrite some of the post-processed files.  So do this as a second step
	npx tsc --project tsconfig.compiler.json --listFilesOnly | grep "src/arr/compiler" \
		| sed s/.ts$$/.js/ | xargs -n1 -I{} realpath --relative-to="src" '{}' | grep -v "\.\." \
		| xargs -n1 -I{} realpath --relative-to="." 'src/{}' \
		| xargs -n1 -I{} perl -0777 -p -i -e 's/(?:;|\A)(\n*)(export \{\}(\n*);?)?\Z/\1/m' '{}'

BUILD_DEPS := \
	src/arr/compiler/pyret-parser.js \
	$(PYRET_JARR)

build: $(BUILD_DEPS)

show-comp: build parser runtime
	npx pyret --checks none -c src/scripts/show-compilation.arr -o show-comp.jarr

typecheck:
	npx tsc --project tsconfig.compiler.json --noEmit --noErrorTruncation --pretty false --incremental false
	npx tsc --project tsconfig.runtime.json --noEmit --noErrorTruncation --pretty false --incremental false


all-tests: build runtime web
	npx jest --verbose

ide-tests: web
	npx jest --verbose "ide"

web-tests: web
	npx jest --verbose "browser"

stopify-web-tests: web
	npx jest --verbose "stopify"

offline-tests: check-block-tests ts-simple-output-tests

simple-output-tests: build runtime
	npx jest --verbose "tests-new/simple-output.test.js"

ts-simple-output-tests: build runtime
	npx jest --setupTestFrameworkScriptFile=./tests-new/ts-pipeline-testing.js --verbose "tests-new/simple-output.test.js"

check-block-tests: build runtime
	npx jest --verbose "tests-new/check-blocks.test.js"

ide: web
	cd ide; npm run start

WEBWORKER_BUILD_DIR := build/worker
WEBWORKER_SRC_DIR := src/webworker
RUNTIME_SRC_DIR := src/runtime
RUNTIME_BUILD_DIR := build/runtime
RUNTIME_JS_SRCS := $(wildcard $(RUNTIME_SRC_DIR)/*.js)
RUNTIME_JSON_SRCS := $(wildcard $(RUNTIME_SRC_DIR)/*.json)
RUNTIME_TS_SRCS := $(wildcard $(RUNTIME_SRC_DIR)/*.ts)
RUNTIME_TSX_SRCS := $(wildcard $(RUNTIME_SRC_DIR)/*.tsx)
RUNTIME_TS_COMPILED_FILES := $(RUNTIME_TS_SRCS:$(RUNTIME_SRC_DIR)/%.ts=$(RUNTIME_BUILD_DIR)/%.js)
RUNTIME_TSX_COMPILED_FILES := $(RUNTIME_TSX_SRCS:$(RUNTIME_SRC_DIR)/%.tsx=$(RUNTIME_BUILD_DIR)/%.js)
RUNTIME_COPIED_BUILTINS := \
	$(RUNTIME_JS_SRCS:$(RUNTIME_SRC_DIR)/%=$(RUNTIME_BUILD_DIR)/%) \
	$(RUNTIME_JSON_SRCS:$(RUNTIME_SRC_DIR)/%=$(RUNTIME_BUILD_DIR)/%)

RUNTIME_ARR_SRC_DIR := src/runtime-arr
# NOTE(joe/ben): An explicitly listed order for things that depend on specific
# load orders; all others get wild-carded at the end.
RUNTIME_ARR_SRCS := \
	$(RUNTIME_ARR_SRC_DIR)/option.arr \
	$(RUNTIME_ARR_SRC_DIR)/either.arr \
	$(RUNTIME_ARR_SRC_DIR)/srcloc.arr \
	$(RUNTIME_ARR_SRC_DIR)/error-display.arr \
	$(RUNTIME_ARR_SRC_DIR)/valueskeleton.arr \
	$(RUNTIME_ARR_SRC_DIR)/lists.arr \
	$(RUNTIME_ARR_SRC_DIR)/error.arr \
	$(RUNTIME_ARR_SRC_DIR)/arrays.arr \
	$(RUNTIME_ARR_SRC_DIR)/pick.arr \
	$(RUNTIME_ARR_SRC_DIR)/color.arr \
	$(RUNTIME_ARR_SRC_DIR)/image-structs.arr \
	$(RUNTIME_ARR_SRC_DIR)/chart.arr \
	$(RUNTIME_ARR_SRC_DIR)/data-source.arr \
	$(RUNTIME_ARR_SRC_DIR)/reactor-events.arr \
	$(wildcard $(RUNTIME_ARR_SRC_DIR)/*.arr)

RUNTIME_ARR_COMPILED_FILES := $(RUNTIME_ARR_SRCS:$(RUNTIME_ARR_SRC_DIR)/%.arr=$(RUNTIME_BUILD_DIR)/%.arr.js)

RUNTIME_PRELUDE_FILES_SRC_DIR := src/runtime-arr-preludes
RUNTIME_PRELUDE_FILES_SRCS := \
	$(RUNTIME_PRELUDE_FILES_SRC_DIR)/empty-context.arr \
	$(RUNTIME_PRELUDE_FILES_SRC_DIR)/essentials2020.arr \
	$(wildcard $(RUNTIME_PRELUDE_FILES_SRC_DIR)/*.arr)
RUNTIME_PRELUDE_COMPILED_FILES := $(RUNTIME_PRELUDE_FILES_SRCS:$(RUNTIME_PRELUDE_FILES_SRC_DIR)/%.arr=$(RUNTIME_BUILD_DIR)/%.arr.js)

STOPIFIED_BUILTINS := \
	$(RUNTIME_JS_SRCS:$(RUNTIME_SRC_DIR)/%.js=$(RUNTIME_BUILD_DIR)/%.js.stopped) \
	$(RUNTIME_TS_SRCS:$(RUNTIME_SRC_DIR)/%.ts=$(RUNTIME_BUILD_DIR)/%.js.stopped) \
	$(RUNTIME_TSX_SRCS:$(RUNTIME_SRC_DIR)/%.ts=$(RUNTIME_BUILD_DIR)/%.js.stopped) \
	$(RUNTIME_ARR_SRCS:$(RUNTIME_ARR_SRC_DIR)/%.arr=$(RUNTIME_BUILD_DIR)/%.arr.js.stopped) \

$(RUNTIME_BUILD_DIR)/%.js : $(RUNTIME_SRC_DIR)/%.ts
	npx tsc --project tsconfig.runtime.json

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
		-no-check-mode \
		--type-check true \
		--compile-mode "builtin-general"
	mv $(RUNTIME_ARR_SRC_DIR)/compiled/project/$*.arr.js $(RUNTIME_BUILD_DIR)
	mv $(RUNTIME_ARR_SRC_DIR)/compiled/project/$*.arr.json $(RUNTIME_BUILD_DIR)


$(RUNTIME_BUILD_DIR)/%.arr.js : $(RUNTIME_PRELUDE_FILES_SRC_DIR)/%.arr
	cd $(RUNTIME_PRELUDE_FILES_SRC_DIR) && node ../../build/phaseA/pyret.jarr \
		--build-runnable $*.arr \
		--builtin-js-dir "$(shell pwd)/$(RUNTIME_BUILD_DIR)" \
		--runtime-builtin-relative-path "./" \
		-no-check-mode \
		--type-check true \
		--compile-mode "builtin-general"
	mv $(RUNTIME_PRELUDE_FILES_SRC_DIR)/compiled/project/$*.arr.js $(RUNTIME_BUILD_DIR)
	mv $(RUNTIME_PRELUDE_FILES_SRC_DIR)/compiled/project/$*.arr.json $(RUNTIME_BUILD_DIR)

RUNTIME_DEPS := \
	$(BUILD_DEPS) \
	$(RUNTIME_SRC_DIR) \
	$(RUNTIME_BUILD_DIR) \
	$(RUNTIME_TS_COMPILED_FILES) \
	$(RUNTIME_TSX_COMPILED_FILES) \
	$(RUNTIME_COPIED_BUILTINS) \
	$(RUNTIME_ARR_COMPILED_FILES) \
	$(RUNTIME_PRELUDE_COMPILED_FILES) \
	$(STOPIFIED_BUILTINS) \

runtime: $(RUNTIME_DEPS)

WORKER_BUILD_DIR := build/worker

$(WORKER_BUILD_DIR):
	mkdir -p $(WORKER_BUILD_DIR)

build/worker/pyret.js: build/worker/pyret-grammar.js src/arr/compiler/pyret-parser.js build/worker/bundled-node-compile-deps.js $(shell pwd)/src/webworker/worker-standalone.js $(WORKER_BUILD_DIR) $(PYRET_JARR_DEPS)
	npx pyret --checks none --standalone-file "$(shell pwd)/src/webworker/worker-standalone.js" --deps-file "$(shell pwd)/build/worker/bundled-node-compile-deps.js" -c src/arr/compiler/webworker.arr -o build/worker/pyret.js

lsp/lsp.js: build/lsp/pyret-grammar.js src/arr/compiler/pyret-parser.js $(PYRET_JARR_DEPS)
	mkdir -p build/lsp/
	npx pyret --checks none --standalone-file "$(shell pwd)/lsp/lsp-standalone.js" -c lsp/lsp-main.arr -o lsp/lsp.js

# NOTE(joe): after this run with node --experimental-specifier-resolution=node lsp/lsp-server.mjs
# Alternatively, never forget to add .js after an import's name!
# https://github.com/microsoft/TypeScript/issues/18442#issuecomment-581738714
lsp/lsp-server.mjs: lsp/lsp.js lsp/lsp-server.ts
	npx tsc --target "esnext" --module "es2015" --moduleResolution "node" lsp/lsp-server.ts
	mv lsp/lsp-server.js lsp/lsp-server.mjs
	npx tsc --target "esnext" --module "es2015" --listFilesOnly lsp/lsp-server.ts \
		| sed s/.ts$$/.js/ | xargs -n1 -I{} realpath --relative-to="src" '{}' | grep -v "\.\." \
		| xargs -n1 -I{} realpath --relative-to="." 'src/{}' \
		| xargs -n1 -I{} perl -0777 -p -i -e 's/(?:;|\A)(\n*)(export \{\}(\n*);?)?\Z/\1/m' '{}'


web: build/worker/pyret.js build/worker/page.html build/worker/main.js

build/worker/runtime-files.json: src/webworker/scripts/runtime-bundler.ts $(RUNTIME_DEPS)
	npx tsc $(WEBWORKER_SRC_DIR)/scripts/runtime-bundler.ts --outDir $(WEBWORKER_BUILD_DIR)
	node $(WEBWORKER_BUILD_DIR)/runtime-bundler.js $(RUNTIME_BUILD_DIR) build/worker/runtime-files.json

build/worker/bundled-node-compile-deps.js: src/js/trove/require-node-compile-dependencies.js
	npx browserify src/js/trove/require-node-compile-dependencies.js -o $@

build/phaseA/pyret-grammar.js: lib/jglr/parser-generator.js src/js/base/pyret-grammar.bnf
	mkdir -p build/phaseA
	mkdir -p build/worker
	node lib/jglr/parser-generator.js src/js/base/pyret-grammar.bnf build/phaseA/pyret-grammar.js "../../lib/jglr" "jglr/jglr" "pyret-base/js/pyret-parser"

src/arr/compiler/pyret-parser.js: build/phaseA/pyret-grammar.js
	node build/phaseA/pyret-grammar.js src/arr/compiler/pyret-parser.js

build/worker/pyret-grammar.js: build/phaseA/pyret-grammar.js
	cp build/phaseA/pyret-grammar.js build/worker/pyret-grammar.js

build/lsp/pyret-grammar.js: build/phaseA/pyret-grammar.js
	cp build/phaseA/pyret-grammar.js build/worker/pyret-grammar.js

parser: src/arr/compiler/pyret-parser.js

build/worker/main.js: src/webworker/*.ts build/worker/runtime-files.json
	npx browserify $(WEBWORKER_SRC_DIR)/main.ts -p [ tsify ] -o $(WEBWORKER_BUILD_DIR)/main.js

build/worker/page.html: src/webworker/page.html
	cp $< $@

clean:
	rm -r -f build/phaseA build/worker
	rm -f src/arr/compiler/pyret-parser.js
	rm -r -f tests-new/.pyret
	rm -r -f .pyret
	rm -r -f build/runtime
	rm -f src/arr/compiler/ts-*.js
	rm -f src/arr/compiler/provide-serialization.js \
			src/arr/compiler/error-display.js

clean-tests:
	rm -r -f tests-new/.pyret

fix-runtime:
	rm -r -f src/runtime-arr/compiled/
	rm -r -f src/runtime-arr-preludes/compiled/
	rm -r -f src/runtime-arr-untyped/compiled/
	rm -r -f build/runtime
	npm run runtime
