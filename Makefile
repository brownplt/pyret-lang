PYRET_COMP0      = build/phase0/pyret.jarr
CLOSURE          = java -jar deps/closure-compiler/compiler.jar
NODE             = node -max-old-space-size=8192
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
DOCS             = docs

# CUSTOMIZE THESE IF NECESSARY
PARSERS         := $(patsubst src/js/base/%-grammar.bnf,src/js/%-parser.js,$(wildcard src/$(JSBASE)/*-grammar.bnf))
COPY_JS          = $(patsubst src/js/base/%.js,src/js/%.js,$(wildcard src/$(JSBASE)/*.js)) \
	src/js/js-numbers.js
COMPILER_FILES = $(wildcard src/arr/compiler/*.arr) $(wildcard src/arr/compiler/locators/*.arr) $(wildcard src/js/trove/*.js) $(wildcard src/arr/trove/*.arr)
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

PHASEA_ALL_DEPS := $(patsubst src/%,$(PHASEA)/%,$(COPY_JS))
PHASEB_ALL_DEPS := $(patsubst src/%,$(PHASEB)/%,$(COPY_JS))
PHASEC_ALL_DEPS := $(patsubst src/%,$(PHASEC)/%,$(COPY_JS))

DOCS_DEPS        = $(patsubst src/%,$(DOCS)/generated/%.rkt,$(SRC_JS) $(TROVE_JS) $(LIBS_JS) $(COPY_JS) $(ROOT_LIBS))
DOCS_SKEL_DEPS   = $(patsubst src/%,$(DOCS)/skeleton/%.rkt,$(SRC_JS) $(LIBS_JS) $(ROOT_LIBS))

PHASEA_DIRS     := $(sort $(dir $(PHASEA_ALL_DEPS)))
PHASEB_DIRS     := $(sort $(dir $(PHASEB_ALL_DEPS)))
PHASEC_DIRS     := $(sort $(dir $(PHASEC_ALL_DEPS)))

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
.PHONY : phaseA
phaseA: $(PHASEA)/pyret.jarr

.PHONY : phaseA-deps
phaseA-deps: $(PYRET_COMPA) $(PHASEA_ALL_DEPS) $(COMPILER_FILES) $(patsubst src/%,$(PHASEA)/%,$(PARSERS))


$(PHASEA)/pyret.jarr: $(PYRET_COMPA) $(PHASEA_ALL_DEPS) $(COMPILER_FILES) $(patsubst src/%,$(PHASEA)/%,$(PARSERS))
	$(NODE) $(PYRET_COMP0) --outfile build/phaseA/pyret.jarr \
                      --build-runnable src/arr/compiler/pyret.arr \
                      --builtin-js-dir build/phase0/trove/ \
                      --builtin-arr-dir src/arr/trove/ \
                      --compiled-dir build/phaseA/compiled/ \
                      -no-check-mode \
                      --require-config src/scripts/standalone-configA-base.json

.PHONY : phaseB
phaseB: $(PHASEB)/pyret.jarr

$(PHASEB)/pyret.jarr: $(PHASEA)/pyret.jarr $(PHASEB_ALL_DEPS) $(patsubst src/%,$(PHASEB)/%,$(PARSERS))
	$(NODE) $(PHASEA)/pyret.jarr --outfile build/phaseB/pyret.jarr \
                      --build-runnable src/arr/compiler/pyret.arr \
                      --builtin-js-dir src/js/trove/ \
                      --builtin-arr-dir src/arr/trove/ \
                      --compiled-dir build/phaseB/compiled/ \
                      -no-check-mode \
                      --require-config src/scripts/standalone-configB.json


.PHONY : phaseC
phaseC: $(PHASEC)/pyret.jarr

$(PHASEC)/pyret.jarr: $(PHASEB)/pyret.jarr $(PHASEC_ALL_DEPS) $(patsubst src/%,$(PHASEC)/%,$(PARSERS))
	$(NODE) $(PHASEB)/pyret.jarr --outfile build/phaseC/pyret.jarr \
                      --build-runnable src/arr/compiler/pyret.arr \
                      --builtin-js-dir src/js/trove/ \
                      --builtin-arr-dir src/arr/trove/ \
                      --compiled-dir build/phaseC/compiled/ \
                      -no-check-mode \
                      --require-config src/scripts/standalone-configC.json

.PHONY : show-comp
show-comp: build/show-compilation.jarr

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
	$(NODE) lib/jglr/parser-generator.js src/$(JSBASE)/$*-grammar.bnf $(PHASEA)/$(JS)/$*-grammar.js "../../../lib/jglr/jglr" "jglr/jglr"
	$(NODE) $(PHASEA)/$(JS)/$*-grammar.js $(PHASEA)/$(JS)/$*-parser.js

$(PHASEB)/$(JS)/%-parser.js: src/$(JSBASE)/%-grammar.bnf src/$(JSBASE)/%-tokenizer.js $(wildcard lib/jglr/*.js)
	$(NODE) lib/jglr/parser-generator.js src/$(JSBASE)/$*-grammar.bnf $(PHASEB)/$(JS)/$*-grammar.js "../../../lib/jglr/jglr" "jglr/jglr"
	$(NODE) $(PHASEB)/$(JS)/$*-grammar.js $(PHASEB)/$(JS)/$*-parser.js


$(PHASEC)/$(JS)/%-parser.js: src/$(JSBASE)/%-grammar.bnf src/$(JSBASE)/%-tokenizer.js $(wildcard lib/jglr/*.js)
	$(NODE) lib/jglr/parser-generator.js src/$(JSBASE)/$*-grammar.bnf $(PHASEC)/$(JS)/$*-grammar.js "../../../lib/jglr/jglr" "jglr/jglr"
	$(NODE) $(PHASEC)/$(JS)/$*-grammar.js $(PHASEC)/$(JS)/$*-parser.js

$(PHASEA)/$(JS)/%.js : src/$(JSBASE)/%.js
	cp $< $@
$(PHASEB)/$(JS)/%.js : src/$(JSBASE)/%.js
	cp $< $@
$(PHASEC)/$(JS)/%.js : src/$(JSBASE)/%.js
	cp $< $@

.PHONY : docs
docs:
	cd docs/written && make VERSION=$(VERSION)

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

.PHONY : old-test
old-test: runtime-test evaluator-test compiler-test pyret-test regression-test type-check-test lib-test

.PHONY : old-test-all
old-test-all: test docs-test benchmark-test

.PHONY : test-all
test-all: test docs-test

.PHONY : test
test: pyret-test type-check-test

.PHONY : runtime-test
runtime-test : $(PYRET_TEST_PREREQ)
	cd tests/runtime/ && PHASE=$(PYRET_TEST_PHASE) $(NODE) test.js require-test-runner/

.PHONY : evaluator-test
evaluator-test: $(PYRET_TEST_PREREQ)
	cd tests/evaluator/ && PHASE=$(PYRET_TEST_PHASE) $(NODE) test.js require-test-runner/

.PHONY : parse-test
parse-test: tests/parse/parse.js build/phaseA/js/pyret-tokenizer.js build/phaseA/js/pyret-parser.js
	cd tests/parse/ && $(NODE) test.js require-test-runner/

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


.PHONY : compiler-test
compiler-test: $(PYRET_TEST_PREREQ)
	$(NODE) $(PYRET_TEST_PHASE)/main-wrapper.js \
    --module-load-dir $(PYRET_TEST_PHASE)/arr/compiler/ \
    -check-all src/arr/compiler/compile.arr

.PHONY : lib-test
lib-test: tests/lib-test-main/lib-test-main.jarr
	$(NODE) tests/lib-test/lib-test-main.jarr

tests/lib-test-main/lib-test-main.jarr: phaseA $(TROVE_ARR_FILES) tests/lib-test/lib-test-main.arr
	$(TEST_BUILD) \
		--build-runnable tests/lib-test/lib-test-main.arr \
    --outfile tests/lib-test/lib-test-main.jarr

.PHONY : benchmark-test
benchmark-test: tools/benchmark/*.js $(PYRET_TEST_PREREQ)
	cd tools/benchmark/ && make test

.PHONY : docs-test
docs-test: docs
	cd docs/written && scribble --htmls index.scrbl

.PHONY : clean
clean:
	$(call RMDIR,$(PHASEA))
	$(call RMDIR,$(PHASEB))
	$(call RMDIR,$(PHASEC))
	$(call RMDIR,build/show-comp/compiled)
	$(call RMDIR,$(RELEASE_DIR))

.PHONY : test-clean
test-clean:
	$(call RMDIR, tests/compiled)

# Written this way because cmd.exe complains about && in command lines
new-bootstrap: no-diff-standalone
	cp $(PHASEC)/pyret.jarr $(PYRET_COMP0)
no-diff-standalone: phaseB phaseC
	diff $(PHASEB)/pyret.jarr $(PHASEC)/pyret.jarr

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
