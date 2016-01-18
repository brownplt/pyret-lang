NODE            := node
NPM             := npm
PROFILE         := profiling.js
CHROME2CALLTREE := lib/chrome2calltree/bin/chrome2calltree.js

#THIS_FILE := 0_empty.arr
THIS_FILE := ast.arr
#THIS_FILE := anf-loop-compiler.arr
#THIS_FILE := adding-ones-2000.arr
#THIS_FILE := recursive-calls-10000.arr
#THIS_FILE := list-set-grow-1000.arr

PYRET_LANG  := ../../
FILE_PREFIX := $(PYRET_LANG)tools/benchmark/auto-report-programs/
PYRET_FILE  := $(FILE_PREFIX)$(THIS_FILE)

PHASE      := phase3
GIT_BRANCH := $(shell git symbolic-ref --short HEAD)
ARTIFACTS  := artifacts/$(GIT_BRANCH).$(PHASE)/

install:
	npm install v8-profiler

cpuprofile: processed.cpuprofile
processed.cpuprofile: $(PYRET_LANG)build/$(PHASE)/$(PHASE).built
	$(NODE) $(PROFILE) $(PYRET_FILE) $(PHASE)
	$(NODE) fix-times.js processed.cpuprofile

callgrind: callgrind.profile
callgrind.profile: processed.cpuprofile
	$(NODE) $(CHROME2CALLTREE) -m -i processed.cpuprofile -o callgrind.profile

archive-profile: callgrind.profile
	mkdir -p $(ARTIFACTS)
	cp callgrind.profile    $(ARTIFACTS)$(THIS_FILE).callgrind.profile

archive-cpuprofile: processed.cpuprofile
	cp processed.cpuprofile $(ARTIFACTS)$(THIS_FILE).processed.cpuprofile

archive-all: archive-cpuprofile archive-profile

auto-report: archive-profile
	rm -f processed.cpuprofile	callgrind.profile

clean:
	rm -f processed.cpuprofile	callgrind.profile