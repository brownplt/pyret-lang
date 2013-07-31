#lang racket/base

(require
 "../runtime.rkt"
 "../string-map.rkt"
 "../ffi-helpers.rkt"
 racket/port)

(define file-dict
  (make-string-map
   (list
    (cons "open-input-file" (ffi-wrap (lambda (filename) (open-input-file filename))))
    (cons "exists-error" (ffi-wrap 'error))
    (cons "exists-append" (ffi-wrap 'append))
    (cons "exists-update" (ffi-wrap 'update))
    (cons "exists-can-update" (ffi-wrap 'can-update))
    (cons "exists-replace" (ffi-wrap 'replace))
    (cons "exists-truncate" (ffi-wrap 'truncate))
    (cons "exists-must-truncate" (ffi-wrap 'must-truncate))
    (cons "exists-truncate-replace" (ffi-wrap 'truncate/replace))
    (cons "open-output-file"
          (ffi-wrap
           (lambda (filename exists)
             (open-output-file filename #:exists exists))))
    (cons "read-line" (ffi-wrap (lambda (f) (read-line f))))
    (cons "read-file" (ffi-wrap (lambda (f) (port->string f))))
    (cons "close-input-file" (ffi-wrap (lambda (f) (close-input-port f))))
    (cons "close-output-file" (ffi-wrap (lambda (f) (close-output-port f))))
    (cons "display" (ffi-wrap (lambda (f val) (display val f)))))))

(define file-obj (p:mk-object file-dict))

(provide (rename-out (file-obj %PYRET-PROVIDE)))
