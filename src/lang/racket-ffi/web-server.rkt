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
              #:extra-files-paths (map ffi-unwrap (p:structural-list->list (p:get-field p:dummy-loc options "static-files")))
              #:server-root-path (ffi-unwrap (p:get-field p:dummy-loc options "server-root-path"))
              #:servlet-current-directory (ffi-unwrap (p:get-field p:dummy-loc options "servlet-current-directory"))
              #:port (ffi-unwrap (p:get-field p:dummy-loc options "port"))
              #:launch-browser? #f))))
        (cons "make-response" (ffi-wrap (lambda (s)
          (response/output (lambda (p) (display s p))))))))))

