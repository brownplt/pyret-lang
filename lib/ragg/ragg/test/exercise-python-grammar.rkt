#lang racket
(require ragg/examples/python-grammar
         ragg/support
         (planet dyoo/python-tokenizer)
         racket/generator
         parser-tools/lex
         racket/match
         rackunit)



(define (adapt-python-tokenizer ip #:end-marker-to-eof? [end-marker-to-eof? #f])
  (define tokens (sequence->generator (generate-tokens ip)))
  (lambda ()
    (let loop ()
      (define next-token (tokens))
      (match next-token
        [(list type text (list start-line start-col) (list end-line end-col) rest-string)
         ;; FIXME: improve the Python tokenizer to hold offsets too.
         (define start-pos (position #f start-line start-col))
         (define end-pos (position #f end-line end-col))
         (position-token (case type
                           [(NAME) 
                            (cond [(hash-has-key? all-tokens-hash (string->symbol text))
                                   (token (string->symbol text) text)]
                                  [else
                                   (token 'NAME text)])]
                           [(OP)
                            (token (string->symbol text) text)]
                           [(NUMBER) 
                            (token 'NUMBER text)]
                           [(STRING) 
                            (token 'STRING text)]
                           [(COMMENT) 
                            (token 'WHITESPACE #:whitespace? #t)]
                           [(NL NEWLINE)
                            (token 'NEWLINE text)]
                           [(DEDENT) 
                            (token 'DEDENT text)]
                           [(INDENT)
                            (token 'INDENT text)]
                           [(ERRORTOKEN)
                            (error 'uh-oh)]
                           [(ENDMARKER) 
                            (if end-marker-to-eof?
                                (token eof)
                                (token 'ENDMARKER text))])
                         start-pos
                         end-pos)]
        [(? void)
         (token eof)]))))
  

(define sample-tokens (adapt-python-tokenizer
                          (open-input-string #<<EOF
import blah
def hello(x):
    print "hello", repr(x)
    blah.baz()

EOF
                                             )))


(void #;pretty-write 
 (syntax->datum (parse "hello.py" sample-tokens)))


(define parse-expr (make-rule-parser expr))


(check-equal?
 (syntax->datum (parse-expr 
                 (adapt-python-tokenizer (open-input-string "42")
                                         #:end-marker-to-eof? #t)))
 '(expr (xor_expr (and_expr (shift_expr (arith_expr (term (factor (power (atom "42"))))))))))



(check-equal?
 (syntax->datum (parse-expr 
                 (adapt-python-tokenizer (open-input-string "(lambda x,y: y,x)")
                                         #:end-marker-to-eof? #t)))
 '(expr (xor_expr (and_expr (shift_expr (arith_expr (term (factor (power (atom "(" (testlist_comp (test (lambdef "lambda" (varargslist (fpdef "x") "," (fpdef "y")) ":" (test (or_test (and_test (not_test (comparison (expr (xor_expr (and_expr (shift_expr (arith_expr (term (factor (power (atom "y")))))))))))))))) "," (test (or_test (and_test (not_test (comparison (expr (xor_expr (and_expr (shift_expr (arith_expr (term (factor (power (atom "x"))))))))))))))) ")"))))))))))


(check-equal?
 (syntax->datum (parse-expr 
                 (adapt-python-tokenizer (open-input-string "sqrt(x^2+y^2)")
                                         #:end-marker-to-eof? #t)))
 '(expr (xor_expr (and_expr (shift_expr (arith_expr (term (factor (power (atom "sqrt") (trailer "(" (arglist (argument (test (or_test (and_test (not_test (comparison (expr (xor_expr (and_expr (shift_expr (arith_expr (term (factor (power (atom "x"))))))) "^" (and_expr (shift_expr (arith_expr (term (factor (power (atom "2")))) "+" (term (factor (power (atom "y"))))))) "^" (and_expr (shift_expr (arith_expr (term (factor (power (atom "2")))))))))))))))) ")"))))))))))
