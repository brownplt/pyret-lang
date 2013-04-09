#lang whalesong

(require (except-in whalesong/lang/whalesong raise pi else))

(provide
  #%module-begin
  #%top-interaction
  #%datum
  #%top
  #%app
  (all-from-out "runtime.rkt")
  [prefix-out r: (all-from-out whalesong/lang/whalesong)]
  ;repl-eval-pyret
  ;print-pyret
  )
  
(require
  ;"eval.rkt"
  "runtime.rkt")

