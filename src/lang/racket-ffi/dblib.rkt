#lang racket/base

(require
  "../runtime.rkt"
  "../string-map.rkt"
  "../ffi-helpers.rkt"
  db)

(define db-dict
  (make-string-map
    (list
      (cons "sqlite3-connect"
            (ffi-wrap
             (lambda (database)
               (sqlite3-connect #:database database #:mode 'create))))
      #;(cons "postgresql-connect" (ffi-wrap
                                  postgresql-connect))
      (cons "query"
            (ffi-wrap
             (lambda (conn stmt args)
               (let [(res (apply query (cons conn (cons stmt args))))]
                 (cond
                  [(simple-result? res) "simple-result"]
                  [(rows-result? res)
                   (map vector->list (rows-result-rows res))])))))

      (cons "disconnect" (ffi-wrap disconnect))
      (cons "is-connected" (ffi-wrap connected?)))))

(define db-obj (p:mk-object db-dict))

(provide (rename-out [db-obj %PYRET-PROVIDE]))
