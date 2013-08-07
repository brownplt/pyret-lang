#lang whalesong

(require
  "../runtime.rkt"
  "../string-map.rkt"
  "../ffi-helpers.rkt"
  (rename-in pyret/lang/pyret-lib/moorings [%PYRET-PROVIDE moorings]))
(provide pyret-map pyret-fold)

(define %list (p:get-field p:dummy-loc moorings "list"))
(define List (p:get-field p:dummy-loc %list "List"))
(define pyret-map (p:get-field p:dummy-loc %list "map"))
(define pyret-filter (p:get-field p:dummy-loc %list "filter"))
(define pyret-fold (p:get-field p:dummy-loc %list "fold"))

