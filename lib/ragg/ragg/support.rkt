#lang racket/base

(provide [struct-out token-struct]
         token
         current-source
         [struct-out exn:fail:parsing]
         current-parser-error-handler
         current-tokenizer-error-handler)

(struct token-struct (type val offset line column span whitespace?) 
        #:transparent)


;; Token constructor.
;; This is intended to be a general token structure constructor that's nice
;; to work with.
;; It should cooperate with the tokenizers constructed with make-permissive-tokenizer.
(define token
  (lambda (type                 ;; (U symbol string)
           [val #f]  ;; any
           #:offset [offset #f] ;; (U #f number)
           #:line [line #f]     ;; (U #f number)
           #:column [column #f] ;; (U #f number)
           #:span [span #f]     ;; boolean
           #:whitespace? [whitespace? #f])
    (token-struct (if (string? type) (string->symbol type) type)
                  val
                  offset line column span whitespace?)))




;; During parsing, we should define the source of the input.
(define current-source (make-parameter #f))



;; When bad things happen, we need to emit errors with source location.
(struct exn:fail:parsing exn:fail (srclocs)
  #:transparent
  #:property prop:exn:srclocs (lambda (instance)
                                (exn:fail:parsing-srclocs instance)))



(define current-parser-error-handler
  (make-parameter
   (lambda (tok-name tok-value offset line col span)
     (raise (exn:fail:parsing
                (format "Encountered parsing error near token ~e (~e) while parsing ~e [line=~a, column=~a, offset=~a]"
                        tok-name tok-value
                        (current-source)
                        line col offset)
                (current-continuation-marks)
                (list (srcloc (current-source) line col offset span)))))))


(define current-tokenizer-error-handler
  (make-parameter
   (lambda (tok-type tok-value offset line column span)
     (raise (exn:fail:parsing
             (format "Encountered unexpected token ~e (~e) while parsing ~e [line=~a, column=~a, offset=~a]"
                     tok-type
                     tok-value
                     (current-source)
                     line column offset)
             (current-continuation-marks)
             (list (srcloc (current-source) line column offset span)))))))

