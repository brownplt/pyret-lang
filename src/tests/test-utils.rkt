#lang racket

(provide
  verbose!
  check-pyret-fail
  check-pyret-exn
  check-pyret-exn/libs
  check-pyret-match
  check-pyret-match/libs
  check-pyret
  check-pyret/libs
  compile-str
  parse-pyret
  eval-pyret
  check-match
  check-not-exn)
(require
 (except-in rackunit check)
   racket/runtime-path
  "../lang/compile.rkt"
  "../lang/tokenizer.rkt"
  "../lang/typecheck.rkt"
  "../lang/desugar.rkt"
  "../lang/runtime.rkt"
  "../lang/load.rkt"
  "../lang/eval.rkt")

;; this insanity is needed in order to get the namespace for pyret
;; (with r: and p: prefixed identifiers) into eval
(module m "../lang/pyret-lang.rkt"
  (r:define-namespace-anchor in-the-module)
  (r:provide in-the-module))
(require 'm)

(define eval-ns (namespace-anchor->namespace in-the-module))

(define verbose #f)
(define (verbose! v) (set! verbose v))
(define (print-test str)
  (when verbose
    (printf "------------------------------------------------------\n")
    (printf "Test for:\n")
    (printf "~a\n" str)))

(define (eval-pyret str)
  (print-test str)
  (eval
   (compile-str str)
    eval-ns))

(define (eval-pyret/libs str)
  (print-test str)
  (eval
   (compile-str/libs str)
    eval-ns))

(define-runtime-path utils-path "test-utils.rkt")

(define (compile-str str)
  (pyret->racket utils-path (open-input-string str)))

(define (compile-str/libs str)
  (pyret->racket/libs utils-path (open-input-string str)))

(define (check-parse-exn str message)
  (check-exn (regexp (regexp-quote message)) (lambda () (parse-pyret str))))

(define-simple-check (check-pyret str expected)
  (equal? (eval-pyret str) expected))

(define-simple-check (check-pyret/libs str expected)
  (equal? (eval-pyret/libs str) expected))

(define-simple-check (check-pyret-fail str expected)
  (not (equal? (eval-pyret str) expected)))

(define-syntax (check-pyret-exn stx)
  (syntax-case stx ()
    [(_ str message)
     (syntax/loc stx
       (check-exn (regexp (regexp-quote message))
            (lambda () (eval-pyret str))))]))

(define-syntax (check-pyret-exn/libs stx)
  (syntax-case stx ()
    [(_ str message)
     (syntax/loc stx
       (check-exn (regexp (regexp-quote message))
            (lambda () (eval-pyret/libs str))))]))

(define-syntax (check-pyret-match stx)
  (syntax-case stx ()
    [(_ str expected)
     (syntax/loc stx (check-match (eval-pyret str) expected))]))

(define-syntax (check-pyret-match/libs stx)
  (syntax-case stx ()
    [(_ str expected)
     (syntax/loc stx (check-match (eval-pyret/libs str) expected))]))

