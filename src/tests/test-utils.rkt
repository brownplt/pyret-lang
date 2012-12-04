#lang racket

(provide parse-pyret eval-pyret check-match)
(require rackunit
         "../lang/compile.rkt"
         "../lang/tokenizer.rkt"
         "../lang/typecheck.rkt"
         "../lang/desugar.rkt"
         "../lang/runtime.rkt")


(dynamic-require "../lang/parser.rkt" 0)
(define ns (module->namespace "../lang/parser.rkt"))

(define-namespace-anchor rt-anchor)
(define rt-ns (namespace-anchor->namespace rt-anchor))

(define (eval-pyret str)
  (eval
    (compile-str str)
    rt-ns))

(define (compile-str str)
  (compile-pyret (typecheck-pyret (desugar-pyret (parse-pyret str)))))

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



