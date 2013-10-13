#lang racket/base

(provide
  bare-read-syntax
  get-info
  (rename-out [my-read read]
              [my-read-syntax read-syntax]))

(require
  racket/runtime-path
  syntax/strip-context
  parser-tools/lex
  ragg/support
  (only-in rnrs/io/ports-6 port-eof?)
  "../parameters.rkt"
  "type-env.rkt"
  "compile.rkt"
  "desugar.rkt"
  "typecheck.rkt"
  "tokenizer.rkt"
  "eval.rkt")

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

(define (get-info in mod line col pos)
  (lambda (key default)
    (case key
      [(color-lexer) get-token]
      [else default])))

(define-runtime-module-path pyret-lang-racket "pyret-lang-racket.rkt")

(define (my-read in)
  (syntax->datum (my-read-syntax #f in)))

; NOTE(joe): Only the first file gets compiled in check mode, so this helper
; returns true the first time, and sets the parameter to be false after
(define (test-check-mode)
  (define old-value (current-check-mode))
  (current-check-mode #f)
  old-value)

(define (bare-read-syntax src in #:check [check #f] #:type-env [env DEFAULT-ENV])
  (cond
    [(port-eof? in) eof]
    [else (strip-context (pyret->racket src in #:check check #:type-env env))]))

(define (my-read-syntax src in)
  (cond
    [(port-eof? in) eof]
    [else
      (with-syntax
         ([pyret-lang-racket-stx
           (path->string (resolved-module-path-name pyret-lang-racket))]
          [src-syntax (src->module-name src)])
            (strip-context
              #`(module src-syntax (file pyret-lang-racket-stx)
                  (r:require (r:only-in racket/base current-read-interaction current-print void))
                  (void (current-read-interaction repl-eval-pyret))
                  (void (current-print (print-pyret #,(current-check-mode))))
                  #,(pyret->racket src in #:toplevel #t #:check (test-check-mode)))))]))

