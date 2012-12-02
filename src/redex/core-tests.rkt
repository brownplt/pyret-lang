#lang racket

(require redex
         "pyret-core.rkt")

(define match-result (term-match πret [(σ Σ e) (term (σ Σ e))]))
  
(define (valid? e)
  (define t (apply-reduction-relation* eval-πret (term (() () ,e))))
  (when (not (equal? (length t) 1))
    (error (format "So many ambiguouses: ~a" t)))
  (define results (match-result (first t)))
  (void (when (not (= (length results) 1))
          (error (format "Resulted in non-term: ~a" (first t))))))

(valid? (term (object ())))

(define nested (term (object (("nested" (object ()))))))

(valid? nested)
(valid? (term (get-field ,nested "nested")))