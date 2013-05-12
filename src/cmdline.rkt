#lang racket/base

(require
  pyret/whalesong/lang/reader
  racket/cmdline
  racket/pretty)

(command-line
  #:once-each
  ("--get-racket" pyret-file "Compile pyret-file to Racket and print it on stdout"
   (pretty-write (syntax->datum (read-syntax 'cmdline (open-input-file pyret-file))))))

