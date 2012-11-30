#lang racket

(require redex
         "pyret-core.rkt")

(define (valid? t)
  
  (when (> (length t) 1) (error (format "So many ambiguouses: ~a" t)))
  ((term-match πret [(σ Σ e) (first t)]) (first t)))

(valid? (apply-reduction-relation
         eval-πret (term (() () (object ())))))

(define nested (term (object (("nested" (object ()))))))

(valid? (apply-reduction-relation*
         eval-πret (term (() () ,nested))))

(valid? (apply-reduction-relation*
         eval-πret (term (() () (get-field ,nested "nested")))))