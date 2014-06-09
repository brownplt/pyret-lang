#lang racket/base

(require scribble/html-properties
         scribble/base
         scribble/core)

(provide setup-math math-in math-disp)

(define mathjax-source "http://cdn.mathjax.org/mathjax/latest/MathJax.js?config=TeX-AMS-MML_HTMLorMML")

(define setup-math
  (paragraph (style #f (list (alt-tag "script")
                             (attributes `((type . "text/javascript")
                                           (src . ,mathjax-source )))))
             '()))

(define (mymath start end . strs)
  (make-element (make-style "relax" '(exact-chars)) `(,start ,@strs ,end)))

(define (math-in . strs) (apply mymath "\\(" "\\)" strs))

(define (math-disp . strs) (apply mymath "\\[" "\\]" strs))

