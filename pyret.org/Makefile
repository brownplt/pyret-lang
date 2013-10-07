# Run Makefile from a fresh directory, such as a site/ subdirectory
# cd site
# make -f ../Makefile
# This keeps the main dir clean, and the subdir nukeable

all:
	scribble ++style ../my-style-changes.css --html ../index.scrbl

pub:
	# Assumed to be running from
	# .Web/Sites/pyret.org/pyret-lang/pyret.org/
	# and need to copy into
	# .Web/Sites/pyret.org/
	scp -r site/* ../..

clean:
	rm -rf site/*