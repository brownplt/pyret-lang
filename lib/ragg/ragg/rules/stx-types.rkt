#lang racket/base
(provide (all-defined-out))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; These are just here to provide bindings for Check Syntax.
;; Otherwise, we should never hit these, as the toplevel rules-codegen
;; should eliminate all uses of these if it does the right thing.
(define (rules stx) (raise-syntax-error #f "Used out of context of rules" stx))
(define (rule stx) (raise-syntax-error #f "Used out of context of rules" stx))
(define (id stx) (raise-syntax-error #f "Used out of context of rules" stx))
(define (lit stx) (raise-syntax-error #f "Used out of context of rules" stx))
(define (token stx) (raise-syntax-error #f "Used out of context of rules" stx))
(define (choice stx) (raise-syntax-error #f "Used out of context of rules" stx))
(define (repeat stx) (raise-syntax-error #f "Used out of context of rules" stx))
(define (maybe stx) (raise-syntax-error #f "Used out of context of rules" stx))
(define (seq stx) (raise-syntax-error #f "Used out of context of rules" stx))