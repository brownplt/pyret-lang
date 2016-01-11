NODE12     := /contrib/bin/node0.12.9


# any file resting in tools/benchmark/auto-report-programs
THIS_FILE := 0_empty.arr
#THIS_FILE := ast.arr
#THIS_FILE := anf-loop-compiler.arr
#THIS_FILE := adding-ones-2000.arr
#THIS_FILE := recursive-calls-10000.arr
#THIS_FILE := list-set-grow-1000.arr

PYRET_LANG := ../../
FILE_PREFIX := $(PYRET_LANG)tools/benchmark/auto-report-programs/
PYRET_FILE := $(FILE_PREFIX)$(THIS_FILE)

PROFILE        := profiling.js

PROCESSOR  := $(PYRET_LANG)node_modules/profiler/nprof
PFLAGS     := --call-graph-size=10

ARTIFACTS  := profiles/

CHROME2CALLTREE := $(PYRET_LANG)node_modules/chrome2calltree/bin/chrome2calltree.js

GIT_BRANCH = $(shell git symbolic-ref --short HEAD)

processed.cpuprofile:
	$(NODE12) $(PROFILE) $(PYRET_FILE)
	$(NODE12) fix-times.js processed.cpuprofile

callgrind.profile: processed.cpuprofile
	$(CHROME2CALLTREE) -i processed.cpuprofile -o callgrind.profile

artifacts: callgrind.profile
	mkdir -p $(ARTIFACTS)$(GIT_BRANCH)
	cp callgrind.profile    $(ARTIFACTS)$(GIT_BRANCH)/$(THIS_FILE).callgrind.profile
	cp processed.cpuprofile $(ARTIFACTS)$(GIT_BRANCH)/$(THIS_FILE).processed.cpuprofile

clean:
	rm -f processed.cpuprofile	callgrind.profile