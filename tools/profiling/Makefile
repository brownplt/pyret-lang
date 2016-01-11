NODE12     := /contrib/bin/node0.12.9

#THIS_FILE := 0_empty.arr
THIS_FILE := adding-ones-2000.arr
#THIS_FILE := recursive-calls-10000.arr
#THIS_FILE := list-set-grow-1000.arr
#THIS_FILE := one-two-three.arr

PYRET_LANG := ../../
FILE_PREFIX := $(PYRET_LANG)tools/benchmark/auto-report-programs/
PYRET_FILE := $(FILE_PREFIX)$(THIS_FILE)

APP        := profiling.js
NFLAGS     := --prof --prof_lazy --log

PROCESSOR  := $(PYRET_LANG)node_modules/profiler/nprof
PFLAGS     := --call-graph-size=10

ARTIFACTS  := profiles/

CHROME2CALLTREE := $(PYRET_LANG)node_modules/chrome2calltree/bin/chrome2calltree.js

processed.cpuprofile:
	$(NODE12) $(APP) $(PYRET_FILE)
	$(NODE12) fix-times.js processed.cpuprofile

callgrind.profile: processed.cpuprofile
	$(CHROME2CALLTREE) -i processed.cpuprofile -o callgrind.profile

artifacts: callgrind.profile
	cp callgrind.profile    $(ARTIFACTS)$(THIS_FILE).callgrind.profile
	cp processed.cpuprofile $(ARTIFACTS)$(THIS_FILE).processed.cpuprofile

clean:
	rm -f processed.cpuprofile	callgrind.profile