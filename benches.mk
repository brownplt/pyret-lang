DEPS := $(shell find ./stopify-bench/*.arr -type f)
JARRS := $(DEPS:.arr=.jarr)
VJARRS := $(DEPS:.arr=.v.jarr)
VSJARRS := $(DEPS:.arr=.vs.jarr)

.PHONY: all
all: $(JARRS) $(VSJARRS) $(VJARRS) ./stopify-bench/vsjarr
	mv ./stopify-bench/*.vs.jarr ./stopify-bench/vsjarr
	mv ./stopify-bench/*.v.jarr ./stopify-bench/vjarr
	mv ./stopify-bench/*.jarr ./stopify-bench/jarr

./stopify-bench/jarr ./stopify-bench/vjarr ./stopify-bench/vsjarr:
	mkdir -p ./stopify-bench/jarr
	mkdir -p ./stopify-bench/vjarr
	mkdir -p ./stopify-bench/vsjarr

%.jarr: %.arr
	$(MAKE) $@

%.vs.jarr: %.arr
	$(MAKE) $@

%.v.jarr: %.arr
	$(MAKE) $@
