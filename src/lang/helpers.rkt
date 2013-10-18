#lang racket

(provide loc-list
         loc+msg->ast-error)

(require "ast.rkt")

(define (loc-list loc)
  (define (serialize-source e)
    (cond
      [(symbol? e) (symbol->string e)]
      [(string? e) e]
      [(path? e) (path->string e)]
      [(false? e) "unknown source"]
      [else (error (format "Non-symbol, non-string, non-path value for
                            source: ~a" e))]))
  (list (serialize-source (srcloc-source loc))
        (srcloc-line loc)
        (srcloc-column loc)
        (srcloc-position loc)
        (srcloc-span loc)))

(define (loc+msg->ast-error loc type msg)
  (define ser (loc-list loc))
  (define path (s-str loc (first ser)))
  (define line (s-num loc (second ser)))
  (define column (s-num loc (third ser)))
  (s-obj loc (list (s-data-field loc (s-str loc "system") (s-bool loc #t))
                   (s-data-field loc (s-str loc "value") (s-obj loc (list (s-data-field loc (s-str loc "type") (s-str loc type))
                                                                          (s-data-field loc (s-str loc "message") (s-str loc msg)))))
                   (s-data-field loc (s-str loc "path") path)
                   (s-data-field loc (s-str loc "line") line)
                   (s-data-field loc (s-str loc "column") column))))
