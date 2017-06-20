PYRET=pyret-lang
CPO=code.pyret.org
ALL_PHASES=phase0 phaseA phaseB phaseC
SHELL=/usr/bin/env bash
NODE_PATH:=$(NODE_PATH):$(PYRET)/node_modules
NODE=node -max-old-space-size=8192
BUILD=build
ARR_SRC=src
RUNNER=carve-seams
PHASE=
COMPILED=
EXTRA_BUILD_DEPS=
NEWEST_PHASE=$(shell tools/newest-phase.sh $(BUILD) $(ALL_PHASES))
PYRET_COMPILER_SRCS=$(wildcard $(PYRET)/src/arr/compiler/*.arr) \
  $(wildcard $(PYRET)/src/arr/compiler/locators/*.arr) \
  $(wildcard $(PYRET)/src/js/trove/*.js) \
  $(wildcard $(PYRET)/src/arr/trove/*.arr)

ARR_SRCS=$(wildcard $(ARR_SRC)/*.arr)

.SUFFIXES += .arr .jarr .template

# check_defined courtesy of http://stackoverflow.com/a/10858332
check_defined = \
    $(strip $(foreach 1,$1, \
        $(call __check_defined,$1,$(strip $(value 2)))))
__check_defined = \
    $(if $(value $1),, \
        $(error Undefined $1$(if $2, ($2))$(if $(value @), \
                required by target '$@')))

ifeq ($(wildcard $(PYRET)),)
$(error Missing symlink to Pyret. \
  Please run 'ln -s <path-to-pyret-lang> ./$(PYRET)'.)
endif

.PHONY: all clean
all:
	$(error A specific target must be specified)

clean:
	rm -r $(BUILD)/compiled-*

.PHONY : phase0 phaseA phaseB phaseC build-program runner

phase0: build-program
phase0: PHASE=phase0
phase0: COMPILED=$(BUILD)/compiled-phase0

phaseA: build-program
phaseA: PHASE=phaseA
phaseA: COMPILED=$(BUILD)/compiled-phaseA

phaseB: build-program
phaseB: PHASE=phaseB
phaseB: COMPILED=$(BUILD)/compiled-phaseB

phaseC: build-program
phaseC: PHASE=phaseC
phaseC: COMPILED=$(BUILD)/compiled-phaseC

$(PYRET)/build/phaseA/js:
	@echo Building phaseA dependencies in pyret-lang
	cd $(PYRET)
	make phaseA-deps

build-deps: | $(PYRET)/build/phaseA/js

build-program: build-deps runner

$(BUILD):
	mkdir -p $(BUILD)

$(BUILD)/compiled-%: $(BUILD)
	mkdir -p $@

runner:
	@:$(call check_defined, PHASE, COMPILED)
	make $(if $(filter $(PHASE), $(NEWEST_PHASE)),,-B) PHASE=$(PHASE) \
	  COMPILED=$(COMPILED) \
	  $(BUILD)/$(RUNNER).jarr

$(BUILD)/$(RUNNER).jarr: $(ARR_SRCS) $(COMPILED)
	@:$(call check_defined, PHASE, COMPILED)
	mkdir -p $(BUILD)/requirejs-$(PHASE)
	$(NODE) $(PYRET)/build/$(PHASE)/pyret.jarr \
	  --builtin-arr-dir $(PYRET)/src/arr/trove \
	  --builtin-arr-dir $(CPO)/src/web/arr/trove \
	  --builtin-js-dir $(PYRET)/src/js/trove \
	  --builtin-js-dir $(CPO)/src/web/js/trove \
	  --require-config <(tools/generate-config.sh $(BUILD)/requirejs-$(PHASE) $(PYRET))  \
	  --standalone-file $(PYRET)/src/js/base/handalone.js \
	  --compiled-dir $(COMPILED) \
	  --build-runnable $(ARR_SRC)/$(RUNNER).arr \
	  -no-check-mode \
	  --outfile $@
	touch $(COMPILED)/built

$(BUILD)/$(RUNNER): tools/$(RUNNER).template
	cp $< $@
	chmod +x $@
