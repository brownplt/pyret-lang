#lang whalesong

; Miscellaneous whalesong functions

(require
   "../runtime.rkt"
   "../ffi-helpers.rkt")

(provide (rename-out [read-sexpr-pfun read-sexpr]))

; read-sexpr: Convert an sexpr string into nested Pyret lists.
;             Symbols are wrapped in ("symbol" ***).
(define (read-sexpr str)
  (define (handle-read-exn x)
    (raise (p:pyret-error
            p:dummy-loc "read-sexpr"
            (format "read-sexpr: Invalid s-expression: \"~a\""
                    str))))
  (define (sexpr->list x)
    (cond [(list? x)   (ffi-wrap (map sexpr->list x))]
          [(symbol? x) (ffi-wrap (list "symbol" (symbol->string x)))]
          [x           (ffi-wrap x)]))
  ;(with-handlers [[(λ (x) #t) #;exn:fail:read?
  ;                 (lambda (_) (handle-read-exn str))]]
    (sexpr->list (parse-expr str)))

  #;(let [[str (ffi-unwrap pyret-val)]]
    (if (not (string? str))
        (raise p:pyret-error p:dummy-loc "read-sexpr-non-string"
               (format "Non-string given to read-sexpr: ~a"
                       (p:to-string pyret-val)))
        (with-handlers [[(λ (x) #t) #;exn:fail:read?
                         (lambda (_) (handle-read-exn pyret-val))]]
          (sexpr->list (parse-expr str)))))

(define read-sexpr-pfun (ffi-wrap read-sexpr))


#| Top-down Parsing |#

(define-struct succ (value input) #:transparent)
(define-struct fail () #:transparent)
(define-struct buffer (str index) #:transparent)

(define (parse-expr str)
  (parse expr str))

(define (parse x str)
  (let [[answer (x (buffer str 0))]]
    (if (succ? answer)
        (succ-value answer)
        (raise "Failed to parse s-expression"))))

(define (eof? input)
  (eq? (string-length (buffer-str input))
       (buffer-index input)))

(define (char pred?)
  (λ (input)
    (if (eof? input)
        (fail)
        (let [[c (string-ref (buffer-str input)
                             (buffer-index input))]]
          (if (pred? c)
              (succ (make-string 1 c)
                    (buffer (buffer-str input)
                            (+ (buffer-index input) 1)))
              (fail))))))

(define (star x [result (list)])
  (λ (input)
    (if (eof? input)
        (succ result input)
        (let [[answer (x input)]]
          (if (succ? answer)
              ((star x (append result (list (succ-value answer))))
               (succ-input answer))
              (succ result input))))))

(define (option . xs)
  (λ (input)
    (if (empty? xs)
        (fail)
        (let [[answer ((car xs) input)]]
          (if (succ? answer)
              answer
              ((apply option (cdr xs)) input))))))

(define (seq . xs)
  (λ (input)
    (if (empty? xs)
        (succ (list) input)
        (let [[answer ((car xs) input)]]
          (if (succ? answer)
              (let [[answers ((apply seq (cdr xs))
                              (succ-input answer))]]
                (if (succ? answers)
                    (succ
                     (cons (succ-value answer)
                           (succ-value answers))
                     (succ-input answers))
                    (fail)))
              (fail))))))

(define (action f x)
  (λ (input)
    (let [[answer (x input)]]
      (if (succ? answer)
          (succ (f (succ-value answer)) (succ-input answer))
          (fail)))))

(define (plus x)
  (action (λ (x) (cons (first x) (second x)))
          (seq x (star x))))

(define-syntax-rule (tie-knot x)
  (λ (input) (x input)))


#| Reading S-expressions |#

(define whitespace
  (star (char char-whitespace?)))

(define num
  (action (λ (x) (string->number (apply string-append x)))
          (plus (char char-numeric?))))

(define string
  (action (λ (x) (apply string-append (second x)))
          (seq (char (λ (c) (eq? c #\")))
               (star (char (λ (c) (not (eq? c #\")))))
               (char (λ (c) (eq? c #\"))))))

(define symbol-chars (string->list "~!@#$%^&*-=_+?,./;:<>|"))
(define (symbol-char? c)
  (or (char-alphabetic? c)
      (member c symbol-chars)))
(define symbol
  (action (λ (x) (string->symbol (apply string-append x)))
          (plus (char symbol-char?))))

(define (token x)
  (action (λ (x) (second x))
          (seq whitespace
               x
               whitespace)))

(define parens
  (action (λ (x) (second x))
          (seq (token (char (λ (c) (eq? c #\())))
               (star (token (tie-knot expr)))
               (token (char (λ (c) (eq? c #\))))))))

(define expr
  (token (option parens string symbol num)))

(define (f x)
  (parse-expr x))

#| Tests
(parse num "3848a9")
(parse string "\"some chars\"blarg")
(parse symbol "symbool gogo")
(parse-expr "3")
(parse-expr "()")
(parse-expr "(( ()394  qqv?#%^fu8   ++ \"st ring\")(  )))")
|#
