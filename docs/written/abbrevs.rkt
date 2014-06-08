#lang racket/base
(require "../scribble-api.rkt")
(provide N No B S RA RA-of L L-of O-of)

(define N (a-id "Number" (xref "<global>" "Number")))
(define No (a-id "Nothing" (xref "<global>" "Nothing")))
(define B (a-id "Boolean" (xref "<global>" "Boolean")))
(define S (a-id "String" (xref "<global>" "String")))
(define RA (a-id "RawArray" (xref "<global>" "RawArray")))
(define (RA-of typ) (a-app (a-id "RawArray" (xref "<global>" "RawArray")) (list typ)))
(define L (a-id "List" (xref "lists" "List")))
(define (L-of typ) (a-app (a-id "List" (xref "lists" "List")) (list typ)))
(define (O-of typ) (a-app (a-id "Option" (xref "option" "Option")) (list typ)))
