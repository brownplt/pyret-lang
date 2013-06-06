#lang racket/base

;; NOTE(joe): This is only necessary to get around an unexplained bug
;; in the interaction of pyret's #lang and Whalesong's module
;; includes.  It precompiles library files from Pyret to Racket so
;; they can be accessed as pure Whalesong files.

(require
  racket/pretty
  "library/lang/reader.rkt")
(provide pre-installer)
(define LIB-BASE "src/lang/pyret-lib/")
(define LIB-FILES (list "list" "error" "builtins" "option" "checkers"))
(define LIBS
  (map (lambda (p) (build-path LIB-BASE p))
       LIB-FILES))

(define (pre-installer base-collects-path pyret-path)
  (for ((lib LIBS))
    (define lib-in (string-append (path->string lib) ".arr"))
    (define lib-out (string-append (path->string lib) ".rkt"))
    (printf "pyret setup: Compiling ~a -> ~a\n" lib-in lib-out)
    (define pyret-file (open-input-file lib-in))
    (define racket-file (open-output-file lib-out #:exists 'replace))
    (pretty-write (syntax->datum (read-syntax 'setup.rkt pyret-file))
                  racket-file)
    (close-output-port racket-file)
    (close-input-port pyret-file)))
