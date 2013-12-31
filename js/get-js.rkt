#lang racket/base

(require
  pyret/lang/runtime
  pyret/lang/ffi-helpers
  pyret/lang/type-env
  (rename-in pyret/lang/racket-ffi/namespaces [%PYRET-PROVIDE namespaces])
  (rename-in "pyret-to-js-direct.arr" [%PYRET-PROVIDE p-to-js]))
(provide pyret-to-js)

(define prog-to-js (p:get-field p:dummy-loc p-to-js "src-program-to-js"))
(define (pyret-to-js src name check ids)
  (define js-and-ids
    ((p:p-base-app prog-to-js)
       (p:mk-str src)
       (p:mk-str name)
       (p:mk-bool check)
       (create-pyret-list (append ids (map symbol->string (map car runtime-env-list))))))
  (hash
    'js-src (p:p-str-s (p:get-field p:dummy-loc js-and-ids "js-src"))
    'ids (map p:p-str-s (pyret-list->list (p:get-field p:dummy-loc js-and-ids "ids")))))

