#lang racket/base

(require (for-syntax racket/base racket/list)
         parser-tools/lex
         ragg/support
         racket/set
         racket/port
         racket/list
         racket/string
         "grammar.rkt")
(provide tokenize)

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
         "graph:" "block:"
         "for" "from"
         "end" ";"))

(define-lex-abbrev
  identifier-chars
  (union #\_ (char-range #\a #\z) (char-range #\A #\Z)))

;; NOTE(dbp): '-' is lexed as misc, so that it can be used as a prefix for
;; numbers as well as for binops
(define-lex-abbrev
  operator-chars
  (union "+" "-" "*" "/" "<=" ">=" "==" "<>" "<" ">" "and" "or" "not" "is" "raises"))

(define (get-middle-pos n pos)
  (position (+ n (position-offset pos))
            (position-line pos)
            (+ n (position-col pos))))

;; NOTE(dbp): actual escape chars are not allowed in strings, but escaped escapes
;; should turn into actual escape characters in the resulting strings.
;; This is a mediocre solution, but I'm not sure of a better way.
(define (fix-escapes s)
  (string-replace
   (string-replace
    (string-replace
     (string-replace
      (string-replace s "\\n" "\n")
      "\\t" "\t")
     "\\r" "\r")
    "\\\"" "\"")
   "\\'" "'"))

(define (tokenize ip)
  (port-count-lines! ip)
  (define-values (p-line p-col p-pos) (port-next-location ip))
  ;; if we are at the beginning, we want to have open-parens be PARENSPACE
  (define at-beginning (and (equal? p-line 1) (equal? p-col 0)))
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
      [(concatenation operator-chars "(")
       (let* [(op (substring lexeme 0
                             (- (string-length lexeme) 1)))
              (op-len (string-length op))
              (middle-pos (get-middle-pos op-len start-pos))]
         (return-without-pos
          (list (position-token (token op op) start-pos middle-pos)
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
      [operator-chars
       (token lexeme lexeme)]
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
      [(concatenation
        "\""
        (repetition 0 +inf.0 (union "\\\"" (char-complement #\")))
        "\"")
       (token STRING (fix-escapes lexeme))]
      [(concatenation
        "'"
        (repetition 0 +inf.0 (union "\\'" (char-complement #\')))
        "'")
       (token STRING (fix-escapes lexeme))]
      ;; brackets
      [(union "[" "]" "{" "}" ")")
       (token lexeme lexeme)]
      ;; whitespace
      [whitespace
       (token WS lexeme #:skip? #t)]
      ;; misc
      [(union "." "!" "," "->" "::" ":" "|" "=>" "^" "=" ":=")
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
      (void)]))
  ;; the queue of tokens to return (can be a list of a single token)
  (define token-queue empty)
  (define (next-token)
    (cond [(cons? token-queue) (let [(tok (first token-queue))]
                                 (set! token-queue (rest token-queue))
                                 tok)]
          [(empty? token-queue) (set! token-queue (my-lexer ip))
                                (next-token)]
          [else (let [(tok token-queue)]
                  (set! token-queue empty)
                  tok)]))
  next-token)
