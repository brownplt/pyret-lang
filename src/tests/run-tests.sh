#!/bin/bash

raco make compile-tests.rkt parse-tests.rkt && racket parse-tests.rkt \
  && racket compile-tests.rkt && echo 'OK'

