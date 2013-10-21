all:
	cd pyret-frog && raco frog -b

serve:
	cd pyret-frog && raco frog -s

pub:
	scp -r site/* browncs:.Web/Sites/pyret.org/

clean:
	rm -rf site/*
