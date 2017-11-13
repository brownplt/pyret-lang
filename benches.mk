DEPS := $(wildcard ./stopify-bench/*.arr)
JARRS := $(DEPS:./stopify-bench/%.arr=./stopify-bench/jarr/%.jarr)
VJARRS := $(DEPS:./stopify-bench/%.arr=./stopify-bench/vjarr/%.v.jarr)
VSJARRS := $(DEPS:./stopify-bench/%.arr=./stopify-bench/vsjarr/%.vs.jarr)

.PHONY: all
all: $(JARRS) $(VSJARRS) $(VJARRS)

stopify-bench/jarr/%.jarr: ./stopify-bench/%.arr
	$(MAKE) ./stopify-bench/$*.jarr

stopify-bench/vjarr/%.v.jarr: ./stopify-bench/%.arr
	$(MAKE) ./stopify-bench/$*.v.jarr

stopify-bench/vsjarr/%.vs.jarr: ./stopify-bench/%.arr
	$(MAKE) ./stopify-bench/$*.vs.jarr

print-%:
	@echo $* = $($*)
