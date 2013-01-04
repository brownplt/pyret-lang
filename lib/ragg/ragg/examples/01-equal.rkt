#lang ragg

## Equal numbers of 0 and 1s in a string.
##
## (Thanks to mithos28 for this one.)


equal : [zero one | one zero]

zero : "0" equal | equal "0"

one : "1" equal | equal "1"
