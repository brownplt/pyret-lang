all:
	cd pyret-frog && raco frog -b
	cd .. && make doc
	rm -rf pyret-frog/docs
	cp -r ../docs/lang/ pyret-frog/docs/

serve:
	cd pyret-frog && raco frog -s

pub:
	scp -r site/* browncs:.Web/Sites/pyret.org/

clean:
	rm -rf site/*
