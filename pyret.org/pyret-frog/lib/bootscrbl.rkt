#lang racket/base

(require scribble/core scribble/html-properties)

(provide (all-defined-out))

(define (pre str)
  (element
    (style #f
           (list
            (alt-tag "pre")
            (attributes (list))))
    str))


(define (div class . content)
  (element
    (style #f
           (list
            (alt-tag "div")
            (attributes
              (list
                (cons 'class class)))))
    content))


(define (container . content)
  (apply div (cons "container" content)))

(define (row . content)
  (apply div (cons "row" content)))

(define (col size . content)
  (define size-str (format "col-md-~a" size))
  (apply div (cons size-str content)))

