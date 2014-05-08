#lang at-exp racket/base

;; Scribble extensions for creating pyret.code.org documentation

(require scribble/base
         scribble/core
         scribble/decode
         scribble/basic
         scribble/html-properties
         )

(provide docmodule
         function
         )

;; render documentation for all definitions in a module
@(define (docmodule name ;#:name (name #f)
                    . defs)
  (section name 
           defs))
  
;; render documentation for a function
@(define (function #:name (name #f)
                   #:contract (contract #f))
  ;; check all required elements provided
  (unless name
    (error "Function definition missing a name field"))
  ;; render the scribble
  name)