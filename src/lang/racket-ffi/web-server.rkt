#lang racket/base

(require
  web-server/servlet
  web-server/servlet-env
  "../runtime.rkt"
  "../ffi-helpers.rkt")
(provide (rename-out [web-server %PYRET-PROVIDE]))

(define web-server
  (p:mk-object
    (make-immutable-hash
      (list
        (cons "serve-servlet"
          (ffi-wrap (lambda (start options)
            (serve/servlet start
              #:servlet-path (ffi-unwrap (p:get-field p:dummy-loc options "servlet-path"))
              #:extra-files-paths (list (ffi-unwrap (p:get-field p:dummy-loc options "static-files")))
              #:launch-browser? #f))))
        (cons "make-response" (ffi-wrap (lambda (s)
          (response/output (lambda (p) (display s p))))))))))
