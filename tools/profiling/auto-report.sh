#!/bin/bash
while read args; do
	make auto-report $args
done<auto-report-jobs.txt