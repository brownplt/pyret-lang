all:
	make pages
	make doc

pages:
	cd pyret-frog && raco frog -b

doc:
	cd .. && make doc
	rm -rf pyret-frog/docs
	cp -r ../docs/lang/ pyret-frog/docs/

serve:
	cd pyret-frog && raco frog -s

stage:
	scp -r pyret-frog/* ssh.cs.brown.edu:/web/cs/web/sites/pyret.org/___staging/
	ssh ssh.cs.brown.edu chmod -R a+r /web/cs/web/sites/pyret.org/___staging/*

pub:
	scp -r pyret-frog/* ssh.cs.brown.edu:/web/cs/web/sites/pyret.org/
	ssh ssh.cs.brown.edu chmod -R a+r /web/cs/web/sites/pyret.org/*

clean:
	rm -rf site/*
