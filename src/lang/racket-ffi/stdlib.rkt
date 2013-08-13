#lang whalesong

(require
  "../runtime.rkt"
  "../string-map.rkt"
  "../ffi-helpers.rkt"
  (rename-in pyret/lang/pyret-lib/moorings [%PYRET-PROVIDE moorings]))
(provide List pyret-map pyret-fold pyret-filter pyret-link pyret-empty)

(define %list (p:get-field p:dummy-loc moorings "list"))
(define List (p:get-field p:dummy-loc %list "List"))
(define pyret-link (p:get-field p:dummy-loc %list "link"))
(define pyret-empty (p:get-field p:dummy-loc %list "empty"))
(define pyret-map (p:get-field p:dummy-loc %list "map"))
(define pyret-filter (p:get-field p:dummy-loc %list "filter"))
(define pyret-fold (p:get-field p:dummy-loc %list "fold"))

