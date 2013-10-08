all:
	# Puts output in site/ to keep current dir clean and subdir nukeable
	(cd site; scribble ++style ../my-style-changes.css --html ../index.scrbl)

pub:
	scp -r site/* browncs:.Web/Sites/pyret.org/

clean:
	rm -rf site/*