DEPS := $(wildcard ./stopify-bench/*.arr)
JARRS := $(DEPS:./stopify-bench/%.arr=./stopify-bench/jarr/%.jarr)
VJARRS := $(DEPS:./stopify-bench/%.arr=./stopify-bench/vjarr/%.v.jarr)
VSJARRS := $(DEPS:./stopify-bench/%.arr=./stopify-bench/vsjarr/%.vs.jarr)

.PHONY: all
all: $(JARRS) $(VSJARRS) $(VJARRS)

./stopify-bench/jarr:
	mkdir -p ./stopify-bench/jarr

./stopify-bench/vjarr:
	mkdir -p ./stopify-bench/vjarr

./stopify-bench/vsjarr:
	mkdir -p ./stopify-bench/vsjarr

stopify-bench/jarr/%.jarr: ./stopify-bench/%.arr | ./stopify-bench/jarr
	$(MAKE) ./stopify-bench/$*.jarr
	mv ./stopify-bench/$*.jarr $@

stopify-bench/vjarr/%.v.jarr: ./stopify-bench/%.arr | ./stopify-bench/vjarr
	$(MAKE) ./stopify-bench/$*.v.jarr
	mv ./stopify-bench/$*.v.jarr $@

stopify-bench/vsjarr/%.vs.jarr: ./stopify-bench/%.arr | ./stopify-bench/vsjarr
	$(MAKE) ./stopify-bench/$*.vs.jarr
	mv ./stopify-bench/$*.vs.jarr $@

print-%:
	@echo $* = $($*)
