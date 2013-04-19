#lang whalesong

(require
  (except-in whalesong/lang/whalesong raise pi else)
  "runtime.rkt"
  )

(provide
  #%module-begin
  #%top-interaction
  #%datum
  #%top
  #%app
  (all-from-out "runtime.rkt")

  [prefix-out r: (all-from-out whalesong/lang/whalesong)]
  )
  
