#lang racket/base

(require (for-syntax racket/base racket/list)
         parser-tools/lex
         ragg/support
         racket/set
         racket/port
         racket/list
         racket/string
         "grammar.rkt")
(provide tokenize fix-escapes pyret-keyword?)

(define KEYWORD 'KEYWORD)
(define NAME 'NAME)
(define NUMBER 'NUMBER)
(define STRING 'STRING)
(define WS 'WS)
(define ENDMARKER 'ENDMARKER)
(define COMMENT 'COMMENT)
(define BACKSLASH 'BACKSLASH)
(define PARENSPACE 'PARENSPACE)
(define PARENNOSPACE 'PARENNOSPACE)
(define OPENSTR 'OPENSTR)
(define BINOP-PLUS 'BINOP-PLUS)
(define BINOP-MINUS 'BINOP-MINUS)
(define BINOP-TIMES 'BINOP-TIMES)
(define BINOP-DIVIDE 'BINOP-DIVIDE)
(define BINOP<= 'BINOP<=)
(define BINOP>= 'BINOP>=)
(define BINOP== 'BINOP==)
(define BINOP<> 'BINOP<>)
(define BINOP< 'BINOP<)
(define BINOP> 'BINOP>)

(define-lex-abbrev
  keywords
  (union "import" "provide" "as"
         "var"
         "fun" "method" "doc:"
         "where:"
         "check:"
         "try:" "except"
         "cases"
         "when" "if" "else if" "else:"
         "data" "with:" "sharing:" "mutable" "cyclic"
         "datatype" "with constructor"
         "graph:" "block:"
         "for" "from"
         "end" ";"))

(define (pyret-keyword? str)
  ((lexer
    [keywords #t]
    [any-string #f])
   (open-input-string (symbol->string str))))

(define-lex-abbrev
  identifier-chars
  (union #\_ (char-range #\a #\z) (char-range #\A #\Z)))

;; NOTE(dbp): '-' is lexed as misc, so that it can be used as a prefix for
;; numbers as well as for binops
(define-lex-abbrev
  word-operator-chars
  (union  "and" "or" "not" "is" "raises"))

(define (get-middle-pos n pos)
  (position (+ n 1 (position-offset pos))
            (position-line pos)
            (+ n 1 (position-col pos))))

(define escapes (pregexp "\\\\([\\\\\"'nrt]|u[0-9A-Fa-f]{1,4}|x[0-9A-Fa-f]{1,2}|[0-7]{1,3}|[\r\n]{1,2})"))
(define specials
  (hash
    "\n" ""
    "\r" ""
    "\n\r" ""
    "\r\n" ""
    "n" "\n"
    "r" "\r"
    "t" "\t"
    "\"" "\""
    "'" "'"
    "\\" "\\"))

(define (fix-escapes s)
  (regexp-replace* escapes s
   (lambda (x match)
     (define m-oct (string->number match 8))
     (define s-oct (if m-oct (string (integer->char m-oct)) #f))
     (define m-hex
       (if (or (string=? (substring match 0 1) "u") (string=? (substring match 0 1) "x"))
           (string->number (substring match 1) 16)
           #f))
     (define s-hex
       (if m-hex (string (integer->char m-hex)) #f))
     (if s-oct s-oct
         (if s-hex s-hex
             (hash-ref specials match))))))
(define-lex-abbrev single-quote-contents
  (repetition 0 +inf.0
    (union
     (concatenation "\\" (repetition 1 3 (char-set "01234567")))
     (concatenation "\\x" (repetition 1 2 (char-set "0123456789abcdefABCDEF")))
     (concatenation "\\u" (repetition 1 4 (char-set "0123456789abcdefABCDEF")))
     (concatenation "\\" (repetition 1 2 (char-set "\r\n")))
     (concatenation "\\" (char-set "nrt\"'\\"))
     (char-complement (union #\\ #\')))))

(define-lex-abbrev double-quote-contents
  (repetition 0 +inf.0
    (union
     (concatenation "\\" (repetition 1 3 (char-set "01234567")))
     (concatenation "\\x" (repetition 1 2 (char-set "0123456789abcdefABCDEF")))
     (concatenation "\\u" (repetition 1 4 (char-set "0123456789abcdefABCDEF")))
     (concatenation "\\" (repetition 1 2 (char-set "\r\n")))
     (concatenation "\\" (char-set "nrt\"'\\"))
     (char-complement (union #\\ #\")))))


(define (tokenize ip name)
  (port-count-lines! ip)
  (define-values (p-line p-col p-pos) (port-next-location ip))
  ;; if we are at the beginning, we want to have open-parens be PARENSPACE
  (define at-beginning
    (or
      (and (equal? p-line 1) (equal? p-col 0))
      (equal? (peek-char ip) #\()))
  ;; used so that after any number of parenthesis, the next
  ;; parenthesis is tokenized as a PARENSPACE, not a PARENNOSPACE
  (define after-paren at-beginning)
  ;; sometimes we want to run an action before a set of cases in the lexer
  (define-syntax (lexer-src-pos-with-actions stx)
    (define (add-action after)
      (lambda (case) (syntax-case case ()
                       [[pattern action] #`[pattern (let [(val action)]
                                                      (begin #,after val))]])))
    (define (process pattern)
      (syntax-case pattern (after-cases)
        [(after-cases action case ...)
         (map (add-action #'action) (append* (map process (syntax-e #'(case ...)))))]
        [[pattern action] (list #'[pattern action])]))
    (syntax-case stx ()
      [(lexer-src-pos! patterns ...)
       #`(lexer-src-pos #,@(append* (map process (syntax-e #'(patterns ...)))))]))
  (define my-lexer
    (lexer-src-pos-with-actions
     ;; open parenthesis: preceded by space and not
     ;; NOTE(dbp): we handle this specially so that we can parse
     ;; function application and binary operators unambiguously. By
     ;; doing this at the tokenizer level, we don't need to deal
     ;; with whitespace in the grammar.
     (after-cases
      (set! after-paren #t)
      ["(("
       (let [(middle-pos (get-middle-pos 1 start-pos))
             (t (if after-paren PARENSPACE PARENNOSPACE))]
         (return-without-pos
          (list (position-token (token t "(") start-pos middle-pos)
                (position-token (token PARENSPACE "(") middle-pos end-pos))))]
      [(concatenation whitespace "(")
       (token PARENSPACE "(")]
      ["("
       (let [(t (if after-paren PARENSPACE PARENNOSPACE))]
         (token t "("))])
     ;; these cases all have after-paren set to false
     (after-cases
      (set! after-paren #f)
      [keywords
       (cond [(set-member? all-token-types (string->symbol lexeme))
              (token (string->symbol lexeme) lexeme)]
             [else
              (token NAME lexeme)])]
      ;; operators
      [word-operator-chars
       (token lexeme lexeme)]
      [(concatenation whitespace "+" whitespace)
       (token BINOP-PLUS "+")]
      [(concatenation whitespace "-" whitespace)
       (token BINOP-MINUS "-")]
      [(concatenation whitespace "*" whitespace)
       (token BINOP-TIMES "*")]
      [(concatenation whitespace "/" whitespace)
       (token BINOP-DIVIDE "/")]
      [(concatenation whitespace "<=" whitespace)
       (token BINOP<= "<=")]
      [(concatenation whitespace ">=" whitespace)
       (token BINOP>= ">=")]
      [(concatenation whitespace "==" whitespace)
       (token BINOP== "==")]
      [(concatenation whitespace "<>" whitespace)
       (token BINOP<> "<>")]
      [(concatenation whitespace "<" whitespace)
       (token BINOP< "<")]
      [(concatenation whitespace ">" whitespace)
       (token BINOP> ">")]
      ;; names
      [(concatenation identifier-chars
                      (repetition 0 +inf.0
                                  (union #\- numeric identifier-chars)))
       ;; NOTE(dbp): we are getting literals from the grammar and making
       ;; them not be NAME anymore.
       (cond [(set-member? all-token-types (string->symbol lexeme))
              (token (string->symbol lexeme) lexeme)]
             [else
              (token NAME lexeme)])]
      ;; numbers
      [(concatenation
        (repetition 1 +inf.0 numeric)
        (union ""
               (concatenation
                #\.
                (repetition 1 +inf.0 numeric))))
       (token NUMBER lexeme)]
      ;; strings
      [(concatenation "\"" double-quote-contents "\"")
       (token STRING (fix-escapes lexeme))]
      [(concatenation "'" single-quote-contents "'")
       (token STRING (fix-escapes lexeme))]
      ;; brackets
      [(union "[" "]" "{" "}" ")")
       (token lexeme lexeme)]
      ;; whitespace
      [whitespace
       (token WS lexeme #:skip? #t)]
      ;; misc
      [(union "." "!" "," "->" "::" ":" "|" "=>" "^" "=" ":=" "<" ">" "-")
       (token lexeme lexeme)]
      ;; comments
      [(concatenation #\# (repetition 0 +inf.0
                                      (char-complement #\newline)))
       (token COMMENT lexeme #:skip? #t)]
      ;; semicolons - for the to-be-removed do notation
      [#\;
       (token lexeme lexeme)]
      ;; backslash - for the to-be-changed lambda notation
      [#\\
       (token BACKSLASH lexeme)])
     ;; match eof
     [(eof)
      (void)]
     [(concatenation "\'" (concatenation single-quote-contents (repetition 0 1 "\\")))
      (token OPENSTR lexeme)]
     [(concatenation "\"" (concatenation double-quote-contents (repetition 0 1 "\\")))
      (token OPENSTR lexeme)]))
  ;; the queue of tokens to return (can be a list of a single token)
  (define token-queue empty)
  (define (next-token)
    (parameterize [(file-path name)]
      (cond [(cons? token-queue) (let [(tok (first token-queue))]
                                   (set! token-queue (rest token-queue))
                                   tok)]
            [(empty? token-queue) (set! token-queue (my-lexer ip))
                                  (next-token)]
            [else (let [(tok token-queue)]
                    (set! token-queue empty)
                    tok)])))
  next-token)
