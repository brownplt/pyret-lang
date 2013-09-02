#lang whalesong

; Miscellaneous whalesong functions

(require
   (except-in "../runtime.rkt" raise)
   "../ffi-helpers.rkt")

(provide (rename-out [read-sexpr-pfun read-sexpr]))

; read-sexpr: Convert an sexpr string into nested Pyret lists.
;             Symbols are wrapped in ("symbol" ***).
(define read-sexpr-pfun
  (p:pλ (pyret-str)
  "Take a string as input, and parse it into an s-expression.
Each s-expression is a number, symbol, string, or a list of
s-expressions surrounded by parenthesis and separated by whitespace.
Parenthesized lists are converted into Pyret lists, and symbols
are converted into a list [\"symbol\", <the-symbol>].

For example, read-sexpr(\"((-13 +14 88.8) cats ++ \\\"dogs\\\")\") will return
  [[-13, 14, 88.8], [\"symbol\", \"cats\"], [\"symbol\", \"++\"], \"dogs\"]
"
  (let [[str (ffi-unwrap pyret-str)]]
  (define (handle-read-exn x)
    (raise (p:pyret-error
            p:dummy-loc "read-sexpr"
            (format "read-sexpr: Invalid s-expression: \"~a\"" str))))
  (define (sexpr->list x)
    (cond [(list? x)   (map sexpr->list x)]
          [(symbol? x) (list "symbol" (symbol->string x))]
          [x           x]))
  (with-handlers [[(λ (x) #t) handle-read-exn]]
    (ffi-wrap (sexpr->list (parse-expr str)))))))

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
  (action (λ (x)
            (let [[sign (first x)]
                  [digits (second x)]]
              (* (if (equal? sign "-") -1 1)
                 (string->number (apply string-append digits)))))
          (seq (option (char (λ (c) (eq? c #\-)))
                       (char (λ (c) (eq? c #\+)))
                       (seq))
               (plus (char (λ (c) (or (char-numeric? c)
                                      (eq? c #\.))))))))

(define string
  (action (λ (x) (apply string-append (second x)))
          (seq (char (λ (c) (eq? c #\")))
               (star (char (λ (c) (not (eq? c #\")))))
               (char (λ (c) (eq? c #\"))))))

(define symbol-chars (string->list "~!@#$%^&*-=_+?,./;:<>|"))
(define (symbol-char? c)
  (or (char-alphabetic? c)
      (char-numeric? c)
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
  (token (option parens string num symbol)))

(define (f x)
  (parse-expr x))

#| Tests |# #|
(parse num "3848a9")
(parse string "\"some chars\"blarg")
(parse symbol "symbool gogo")
(parse-expr "3")
(parse-expr "()")
(parse-expr "(( ()394  qqv?#%^fu8   ++ \"st ring\")(  )))")
(parse-expr "+")
(parse-expr "++")
(parse-expr "-385")
(parse-expr "- 385")
(parse-expr "3.48")
(parse-expr "+3.48")
|#