#!/bin/bash

# make sure we are in the test directory before running!
cd $(dirname $(realpath $0))
# run all tests
raco make compile-tests.rkt parse-tests.rkt type-tests.rkt well-formed-tests.rkt indentation-tests.rkt \
  && racket parse-tests.rkt \
  && racket compile-tests.rkt \
  && racket type-tests.rkt \
  && racket well-formed-tests.rkt \
  && racket indentation-tests.rkt
