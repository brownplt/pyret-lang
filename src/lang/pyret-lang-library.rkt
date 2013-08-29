#lang whalesong

(require
  (except-in whalesong/lang/whalesong raise pi else gensym)
  "runtime.rkt"
  )

(provide
  #%module-begin
  #%top-interaction
  #%datum
  #%top
  #%app

  [prefix-out r: (all-from-out whalesong/lang/whalesong)]
  (all-from-out "runtime.rkt")
  )
  
