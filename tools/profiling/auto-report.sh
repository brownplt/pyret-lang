while read args; do
	make artifacts-and-cleanup $args
done<jobs.txt