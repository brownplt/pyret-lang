all:	pyret-sugar-rules.js stepify-sugar-rules.js


pyret-sugar-rules.js:	make-sugar-rules.py pyret-sugar-rules.js.template ../../arr/trove/pyret.sugar
	python make-sugar-rules.py > $@


stepify-sugar-rules.js:	make-stepify-sugar-rules.py pyret-sugar-rules.js.template ../../arr/trove/stepify.sugar
	python make-stepify-sugar-rules.py > $@
