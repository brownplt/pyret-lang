#!/bin/bash
while read args; do
	make auto-report $args
done<jobs.txt