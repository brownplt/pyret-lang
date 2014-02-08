#lang whalesong

(require
  (except-in whalesong/lang/whalesong list error raise pi else gensym)
  (prefix-in r: (only-in whalesong/lang/whalesong list error raise))
  "racket-ffi/animate.rkt"
  "racket-ffi/stdlib.rkt"
  (except-in "runtime.rkt" ___set-link ___set-empty)
  (rename-in pyret/lang/pyret-lib/moorings [%PYRET-PROVIDE moorings])
  )

(define list (p:get-field p:dummy-loc moorings "list"))
(define error (p:get-field p:dummy-loc moorings "error"))
(define builtins (p:get-field p:dummy-loc moorings "builtins"))
(define checkers (p:get-field p:dummy-loc moorings "checkers"))
(define option (p:get-field p:dummy-loc moorings "option"))
(define cs173 (p:get-field p:dummy-loc moorings "cs173"))

;; WARNING: Did you remember to update src/lang/type-env.rkt?
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
  cs173

  (rename-out
    [pyret-List List]
    [pyret-is-empty is-empty]
    [pyret-is-link is-link]
    [pyret-empty empty]
    [pyret-link link]
    [pyret-range range]
    [pyret-range-by range-by]
    [pyret-repeat repeat]
    [pyret-filter filter]
    [pyret-partition partition]
    [pyret-split-at split-at]
    [pyret-any any]
    [pyret-find find]
    [pyret-map map]
    [pyret-map2 map2]
    [pyret-map3 map3]
    [pyret-map4 map4]
    [pyret-map_n map_n]
    [pyret-map2_n map2_n]
    [pyret-map3_n map3_n]
    [pyret-map4_n map4_n]
    [pyret-each each]
    [pyret-each2 each2]
    [pyret-each3 each3]
    [pyret-each4 each4]
    [pyret-each_n each_n]
    [pyret-each2_n each2_n]
    [pyret-each3_n each3_n]
    [pyret-each4_n each4_n]
    [pyret-fold fold]
    [pyret-fold2 fold2]
    [pyret-fold3 fold3]
    [pyret-fold4 fold4]
    [pyret-index index]
    [pyret-read-sexpr read-sexpr]
    [pyret-read-sexpr-list read-sexpr-list]
    [pyret-Option Option]
    [pyret-is-none is-none]
    [pyret-is-some is-some]
    [pyret-none none]
    [pyret-some some]

    [pyret-sets sets]
    [pyret-Set Set]
    [pyret-set set]
    [pyret-list-set list-set]
    [pyret-tree-set tree-set]

    [pyret-array array]
    [pyret-array-get array-get]
    [pyret-array-set array-set]
    [pyret-array-length array-length]
    [pyret-array-to-list array-to-list]

    [pyret-identical identical]
    [pyret-string-to-list string-to-list])

  (rename-out
    [pyret-random random])

  (rename-out
    [p:string-append string-append]
    [p:string-length string-length]
    
    [p:plus plus]
    [p:minus minus]
    [p:times times]
    [p:divide divide]

    [p:sqrt sqrt]
    [p:expt expt]
    [p:sq sq])

  (all-from-out "racket-ffi/animate.rkt")

  [prefix-out r: (all-from-out whalesong/lang/whalesong)]
  r:list r:error r:raise
  (all-from-out "runtime.rkt")
  )
