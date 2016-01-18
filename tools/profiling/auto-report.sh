#!/bin/bash
while read args; do
	make auto-report $args NODE=/contrib/bin/node0.12.9
done<auto-report-jobs.txt