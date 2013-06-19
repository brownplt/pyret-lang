#lang racket

(provide
  verbose!
  check-exn
  check-pyret-fail
  check-pyret-exn
  check-pyret-match
  check-pyret-match/check
  check-pyret
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
 "../lang/load.rkt"
 "../lang/eval.rkt")

(define verbose #f)
(define (verbose! v) (set! verbose v))
(define (print-test str)
  (when verbose
    (printf "------------------------------------------------------\n")
    (printf "Test for:\n")
    (printf "~a\n" str)))

(module test-shell "../lang/pyret-lang-racket.rkt"
  (r:define-namespace-anchor test-shell-anchor)
  (r:provide test-shell-anchor))
(require (submod "." test-shell))

(define-runtime-path pyret-lang "../lang/pyret-lang-racket.rkt")

(define (make-fresh-namespace)
  (define ns (namespace-anchor->empty-namespace test-shell-anchor))
  (parameterize ([current-namespace ns])
    (namespace-require pyret-lang)
    (void))
  ns)

(define (py-eval stx)
  (eval stx (make-fresh-namespace)))

(define (eval-pyret str)
  (print-test str)
  ;; NOTE(dbp): because we expect there to be whitespace before paren exprs,
  ;; in test context (where there is no #lang), we prepend everything with " "
  (py-eval (compile-str (string-append " " str))))

(define (eval-pyret/check str)
  (print-test str)
  (py-eval (compile-str/check (string-append " " str))))

(define (eval-pyret/check/lib str)
  (print-test str)
  (py-eval (compile-str/check (string-append " " str))))

(define-runtime-path utils-path "test-utils.rkt")

(define (compile-str str)
  (pyret->racket utils-path (open-input-string str)))

(define (compile-str/check str)
  (pyret->racket utils-path (open-input-string str) #:check #t))

(define (check-parse-exn str message)
  (check-exn (regexp (regexp-quote message)) (lambda () (parse-pyret str))))

(define-simple-check (check-pyret str expected)
  (equal? (eval-pyret str) expected))

(define-simple-check (check-pyret-fail str expected)
  (not (equal? (eval-pyret str) expected)))

(define-syntax (check-pyret-exn stx)
  (syntax-case stx ()
    [(_ str message)
     (syntax/loc stx
       (check-exn (regexp (regexp-quote message))
            (lambda () (eval-pyret str))))]))

(define-syntax (check-pyret-match stx)
  (syntax-case stx ()
    [(_ str expected)
     (syntax/loc stx (check-match (eval-pyret str) expected))]))

(define-syntax (check-pyret-match/check stx)
  (syntax-case stx ()
    [(_ file expected-value t p f te oe)
     (quasisyntax/loc stx
       (let ()
         (print-test (format "~a" file))
         (define-values (base name dir?)
           (split-path (simplify-path (path->complete-path file))))
         (define output (open-output-string))
         (define result
           (parameterize ([current-output-port output]
                          [current-load-relative-directory base])
             (eval-pyret/check (port->string (open-input-file file)))))
         (define stdout (get-output-string output))
         (define expected-stdout
           (format
            "Total: ~a, Passed: ~a, Failed: ~a, Errors in tests: ~a, Errors in between tests: ~a" t p f te oe))
         #,(quasisyntax/loc stx
             (match result
             [expected-value #t]
             [_ #,(syntax/loc stx (check-match result expected-value))]))
         #,(quasisyntax/loc stx
             (if (regexp-match (regexp-quote expected-stdout) stdout)
             (map (λ (_) (check-true #t)) (range t))
             #,(syntax/loc stx
               (check-regexp-match (regexp-quote expected-stdout)
                                 stdout))))))]))

