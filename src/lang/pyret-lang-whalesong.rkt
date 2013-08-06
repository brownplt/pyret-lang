#lang whalesong

(require
  (except-in whalesong/lang/whalesong list error raise pi else)
  (prefix-in r: (only-in whalesong/lang/whalesong list error raise))
  "racket-ffi/animate.rkt"
  "runtime.rkt"
  (rename-in pyret/lang/pyret-lib/moorings [%PYRET-PROVIDE moorings])
  )

(define list (p:get-field p:dummy-loc moorings "list"))
(define error (p:get-field p:dummy-loc moorings "error"))
(define builtins (p:get-field p:dummy-loc moorings "builtins"))
(define checkers (p:get-field p:dummy-loc moorings "checkers"))
(define option (p:get-field p:dummy-loc moorings "option"))

(define List (p:get-field p:dummy-loc list "List"))
(define pyret-map (p:get-field p:dummy-loc list "map"))
(define pyret-filter (p:get-field p:dummy-loc list "filter"))
(define pyret-fold (p:get-field p:dummy-loc list "fold"))

(provide
  #%module-begin
  #%top-interaction
  #%datum
  #%top
  #%app

  list
  List
  (rename-out [pyret-map map]
              [pyret-filter filter]
              [pyret-fold fold])
  error
  builtins
  checkers
  option

  (all-from-out "racket-ffi/animate.rkt")
  (rename-out [pyret-random random])

  [prefix-out r: (all-from-out whalesong/lang/whalesong)]
  r:list r:error r:raise
  (all-from-out "runtime.rkt")
  )
