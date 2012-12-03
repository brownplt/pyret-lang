#lang racket

(require
  rackunit
  redex
  "../lang/runtime.rkt"
  "../tests/test-utils.rkt"
  "pyret-core.rkt"
  "redex-compile.rkt")

(define (compile-to-redex str)
  (redex-compile-pyret (parse-pyret str)))

(define (valid? e)
  (define t (apply-reduction-relation* eval-πret (term (() () ,e))))
  (when (> (length t) 1) (error (format "So many ambiguouses: ~a" t)))
  ((term-match πret [(σ Σ e) (first t)]) (first t)))

(valid? (compile-to-redex "{}"))
(compile-to-redex "{nested:{}}.nested")
(valid? (compile-to-redex "{nested:{}}.nested"))

(define (get-answer str)
  (first (valid? (compile-to-redex str))))

(define (check-answer val code)
  ((redex-compile-answer val) (get-answer code)))

"Should not match"
(check-answer (mk-str "foo") "'nested'")
"Should match"
(check-answer (mk-str "nested") "'nested'")

"Should match"
(check-answer (mk-num 52) "52")
"Should not match"
(check-answer (mk-num 51) "52")

"Should not match"
(check-answer (mk-object (make-hash)) "{x:5}")
"Should not match"
(check-answer (mk-object (make-hash (list (cons "x" (mk-num 5))))) "{}")
"Should match"
(check-answer (mk-object (make-hash)) "{}")
"Should match"
(check-answer (mk-object (make-hash (list (cons "x" (mk-num 5))))) "{x:5}")
"Should match"
(check-answer (mk-object (make-hash (list (cons "nested" (mk-object (make-hash)))))) "{nested:{}}")
"Should match"
(check-answer (mk-object (make-hash (list (cons "first" (mk-object (make-hash)))
                                          (cons "second" (mk-object (make-hash))))))
              "{second:{}, first:{}}")
"Should match"
(check-answer (mk-object (make-hash)) "{} {}")


(extract-pyret-val (get-answer "{nested:{}}"))