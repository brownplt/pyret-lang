#lang racket/base

(require
  rackunit
  rackunit/text-ui
  racket/file
  profile
	 "test-utils.rkt"
	 "../lang/runtime.rkt")

(define PROFILE? #f)
(compile-context-preservation-enabled #t)

(define (benchmark-path sub) (build-path "benchmarks/" sub))
(define (benchmark-run filename passing)
  (printf "Running ~a\n" filename)
  (define full-path (benchmark-path filename))
  (define-values (base name _) (split-path (simplify-path full-path)))
  (define racket-file (string-append (path->string name) ".rkt"))
  (define pyret-file (string-append (path->string name) ".arr"))
  (parameterize [(current-directory base)]
    (define (run-test)
      (eval-pyret/check (file->string pyret-file))
      #;(check-pyret-match/check pyret-file _ passing passing 0 0 0))
    (time (dynamic-require racket-file #f))
    (if PROFILE?
        (time (profile-thunk run-test #:delay 0.01 #:threads #t))
        (time (run-test)))))

(define all (test-suite "all"

(benchmark-run "list-creation" 0)
(benchmark-run "list-equality" 1)
(benchmark-run "many-fields" 1)
(benchmark-run "match" 0)

))

(run-tests all 'normal)

