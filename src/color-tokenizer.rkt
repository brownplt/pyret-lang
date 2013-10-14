#lang racket/base

(require 
  "lang/tokenizer.rkt"
  parser-tools/lex
  ragg/support)
(provide get-token)

(define TOKENIZERS (make-hash))
(define (get-token ip offset last-mode)
  (define tok (hash-ref TOKENIZERS ip
    (lambda ()
      (define this-tok (tokenize ip))
      (hash-set! TOKENIZERS ip this-tok)
      this-tok)))
  (define t
    (with-handlers
      [(exn:fail? (lambda (e) (position-token (void) #f #f)))]
      (tok)))
  (define (token->name token)
    (cond
      [(member (symbol->string token) '("(" ")" "[" "]" "{" "}")) 'parenthesis]
      [else
        (case token
         [(KEYWORD) 'keyword]
         [(COMMENT) 'comment]
         [(STRING) 'string]
         [(NUMBER) 'constant]

         [(PARENSPACE) 'parenthesis]
         [(PARENNOSPACE) 'parenthesis]

         [(WS) 'whitespace]

         [(NAME) 'other]
         
         [else 'other])]))
  (define (bracket val)
    (cond
      [(member val '("(" ")" "[" "]" "{" "}"))
       (string->symbol val)]
      [else #f]))
  (define inner-token (position-token-token t))
  (cond
    [(void? inner-token)
     (hash-remove! TOKENIZERS ip)
     (values 'done 'eof #f #f #f 0 'no-mode)]
    [(pyret-keyword? (token-struct-type inner-token))
     (values
       (token-struct-val inner-token)
       'keyword
       #f
       (position-offset (position-token-start-pos t))
       (position-offset (position-token-end-pos t))
       (string-length (token-struct-val inner-token))
       'in-name)]
    [(equal? (token-struct-val inner-token) 'OPENSTR)
     (values
       (token-struct-val inner-token)
       'other
       #f
       (position-offset (position-token-start-pos t))
       (position-offset (position-token-end-pos t))
       (string-length (token-struct-val inner-token))
       'in-string)]
    [else
     (values
       (token-struct-val inner-token)
       (token->name (token-struct-type (position-token-token t)))
       (bracket (token-struct-val inner-token))
       (position-offset (position-token-start-pos t))
       (position-offset (position-token-end-pos t))
       0
       'normal)]))

