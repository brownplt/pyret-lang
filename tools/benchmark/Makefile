NODE    = node
MODULES = ../../node_modules/
JSHINT  = $(MODULES)jshint/bin/jshint
FILES   = $(shell find . -name \*.js)

jshint:
	$(NODE) $(JSHINT) $(FILES)

test:
	cd tests/ && $(NODE) test.js require-test-runner/