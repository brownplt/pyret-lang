NODE       = node
MODULES    = ../../node_modules/
JSHINT     = $(MODULES)jshint/bin/jshint
FILES      = $(shell find . -name \*.js)
AUTOREPORT = auto-report.js
CSV_FILE   = auto-report.csv

auto-report:
	$(NODE) $(AUTOREPORT) > $(CSV_FILE)

jshint:
	$(NODE) $(JSHINT) $(FILES)

test:
	cd tests/ && $(NODE) test.js require-test-runner/