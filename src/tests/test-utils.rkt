#lang racket

(provide
  check-pyret-fail
  check-pyret-exn
  check-pyret-match
  check-pyret
  compile-str
  parse-pyret
  eval-pyret
  check-match)
(require
 (except-in rackunit check)
   racket/runtime-path
  "../lang/compile.rkt"
  "../lang/tokenizer.rkt"
  "../lang/typecheck.rkt"
  "../lang/desugar.rkt"
  "../lang/runtime.rkt"
  "../lang/eval.rkt")


(dynamic-require "../lang/parser.rkt" 0)
(define ns (module->namespace "../lang/parser.rkt"))


;; this insanity is needed in order to get the namespace for pyret
;; (with r: and p: prefixed identifiers) into eval
(module m "../lang/pyret-lang.rkt"
  (r:define-namespace-anchor in-the-module)
  (r:provide in-the-module))
(require 'm)

(define eval-ns (namespace-anchor->namespace in-the-module))

(define (eval-pyret str)
  (eval
   (compile-str str)
    eval-ns))

(define (compile-str str)
  (pyret->racket "test-utils" (open-input-string str)))

;; note - using eval-syntax below misses an important "enrichment" step:
;; http://docs.racket-lang.org/reference/eval.html?q=eval-syntax&q=eval-syntax&q=%23%25datum#(def._((quote._~23~25kernel)._eval-syntax))
;;
;; NB(joe):  I have no idea what that means
(define (parse-pyret str)
  (eval
   (get-syntax "parse-tests.rkt" (open-input-string str))
   ns))

(define (check-parse-exn str message)
  (check-exn (regexp (regexp-quote message)) (lambda () (parse-pyret str))))

;; NOTE(jpolitz): This match form isn't eager like the others, hence the
;; define-syntax and the need to carry around location information
(define-syntax (check-match stx)
  (syntax-case stx ()
    [(_ actual expected pred)
     (quasisyntax
      (let ([actual-val actual])
       (with-check-info*
        (list (make-check-name 'check-match)
              (make-check-location
               (list '(unsyntax (syntax-source stx))
                     '(unsyntax (syntax-line stx))
                     '(unsyntax (syntax-column stx))
                     '(unsyntax (syntax-position stx))
                     '(unsyntax (syntax-span stx))))
              (make-check-expression '#,(syntax->datum stx))
              (make-check-actual actual-val)
              (make-check-expected 'expected))
        (lambda ()
         (check-true (match actual-val
                       [expected pred]
                       [_ #f]))))))]
    [(_ actual expected)
     (syntax/loc stx (check-match actual expected #t))]))


(define-simple-check (check-pyret str expected)
  (equal? (eval-pyret str) expected))

(define-simple-check (check-pyret-fail str expected)
  (not (equal? (eval-pyret str) expected)))

(define (check-pyret-exn str message)
  (check-exn (regexp (regexp-quote message))
	     (lambda () (eval-pyret str))))

(define-syntax check-pyret-match
  (syntax-rules ()
    [(_ str expected)
      (check-match (eval-pyret str) expected)]))

