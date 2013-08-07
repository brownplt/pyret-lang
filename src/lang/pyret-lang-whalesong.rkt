#lang whalesong

(require
  (except-in whalesong/lang/whalesong list error raise pi else)
  (prefix-in r: (only-in whalesong/lang/whalesong list error raise))
  "racket-ffi/animate.rkt"
  "racket-ffi/stdlib.rkt"
  "runtime.rkt"
  (rename-in pyret/lang/pyret-lib/moorings [%PYRET-PROVIDE moorings])
  )

(define list (p:get-field p:dummy-loc moorings "list"))
(define error (p:get-field p:dummy-loc moorings "error"))
(define builtins (p:get-field p:dummy-loc moorings "builtins"))
(define checkers (p:get-field p:dummy-loc moorings "checkers"))
(define option (p:get-field p:dummy-loc moorings "option"))

(provide
  #%module-begin
  #%top-interaction
  #%datum
  #%top
  #%app

  list

  error
  builtins
  checkers
  option

  (rename-out
    [pyret-filter filter]
    [pyret-fold fold]
    [pyret-map map]
    [pyret-random random])
  (all-from-out "racket-ffi/stdlib.rkt")
  (all-from-out "racket-ffi/animate.rkt")

  [prefix-out r: (all-from-out whalesong/lang/whalesong)]
  r:list r:error r:raise
  (all-from-out "runtime.rkt")
  )
