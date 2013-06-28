#lang racket/base

(require
  web-server/templates
  "../ffi-helpers.rkt"
  "../string-map.rkt"
  "../runtime.rkt")

(provide (rename-out [template %PYRET-PROVIDE]))

(define (render-template filename obj)
  (define dict (p:get-dict obj))
  (define namespace-for-template (make-empty-namespace))
  (namespace-attach-module (current-namespace) 'web-server/templates namespace-for-template)
  (string-map-map dict
    (lambda (key value)
      (define name-of-identifier (string->symbol key))
      (namespace-set-variable-value!
        name-of-identifier
        (ffi-unwrap value)
        #f
        namespace-for-template)))
  (parameterize [(current-namespace namespace-for-template)]
    (namespace-require 'web-server/templates))
  (define to-eval #`(include-template #,(datum->syntax #'render-template filename)))
  (eval to-eval namespace-for-template))

(define template
  (p:mk-object (make-string-map
    (list
      (cons "render" (ffi-wrap render-template))))))

