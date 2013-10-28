#lang whalesong

(require
  "../runtime.rkt"
  "../string-map.rkt"
  "../ffi-helpers.rkt"
  "whalesong-lib.rkt"
  (rename-in pyret/lang/pyret-lib/moorings [%PYRET-PROVIDE moorings]))
(provide (all-defined-out))

(define (get-list-lib name)
  (p:get-field p:dummy-loc %list name))

(define (get-set-lib name)
  (p:get-field p:dummy-loc %set name))

(define %list (p:get-field p:dummy-loc moorings "list"))
(define %set (p:get-field p:dummy-loc moorings "sets"))
(define pyret-List (get-list-lib "List"))
(define pyret-is-empty (get-list-lib "is-empty"))
(define pyret-is-link (get-list-lib "is-link"))
(define pyret-empty (get-list-lib "empty"))
(define pyret-link (get-list-lib "link"))
(define pyret-range (get-list-lib "range"))
(define pyret-range-by (get-list-lib "range-by"))
(define pyret-repeat (get-list-lib "repeat"))
(define pyret-filter (get-list-lib "filter"))
(define pyret-partition (get-list-lib "partition"))
(define pyret-any (get-list-lib "any"))
(define pyret-find (get-list-lib "find"))
(define pyret-map (get-list-lib "map"))
(define pyret-map2 (get-list-lib "map2"))
(define pyret-map3 (get-list-lib "map3"))
(define pyret-map4 (get-list-lib "map4"))
(define pyret-map_n (get-list-lib "map_n"))
(define pyret-map2_n (get-list-lib "map2_n"))
(define pyret-map3_n (get-list-lib "map3_n"))
(define pyret-map4_n (get-list-lib "map4_n"))
(define pyret-each (get-list-lib "each"))
(define pyret-each2 (get-list-lib "each2"))
(define pyret-each3 (get-list-lib "each3"))
(define pyret-each4 (get-list-lib "each4"))
(define pyret-each_n (get-list-lib "each_n"))
(define pyret-each2_n (get-list-lib "each2_n"))
(define pyret-each3_n (get-list-lib "each3_n"))
(define pyret-each4_n (get-list-lib "each4_n"))
(define pyret-fold (get-list-lib "fold"))
(define pyret-fold2 (get-list-lib "fold2"))
(define pyret-fold3 (get-list-lib "fold3"))
(define pyret-fold4 (get-list-lib "fold4"))
(define pyret-index (get-list-lib "index"))
(define pyret-Set (get-set-lib "Set"))
(define pyret-set (get-set-lib "set"))

(define %option (p:get-field p:dummy-loc moorings "option"))
(define (get-option-lib name)
  (p:get-field p:dummy-loc %option name))

(define pyret-Option (get-option-lib "Option"))
(define pyret-is-none (get-option-lib "is-none"))
(define pyret-is-some (get-option-lib "is-some"))
(define pyret-none (get-option-lib "none"))
(define pyret-some (get-option-lib "some"))

(define pyret-read-sexpr %read-sexpr)
(define pyret-read-sexpr-list %read-sexpr-list)

(define (get-builtin-lib name)
  (p:get-field p:dummy-loc %builtins name))
(define %builtins (p:get-field p:dummy-loc moorings "builtins"))
(define pyret-identical (get-builtin-lib "identical"))
(define pyret-string-to-list (get-builtin-lib "string-to-list"))

