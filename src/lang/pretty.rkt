#lang racket/base

(require
  racket/match
  racket/string
  racket/function
  racket/list
  "ast.rkt")

(provide
  pretty
  pretty-ann)

(define tab-size 2)

(define (vary-pretty ast ind)
  (define (pretty ast) (vary-pretty ast ind))
  (define (prettier ast) (vary-pretty ast (+ ind 1)))
  (define (indent [depth 1]) (make-string (* tab-size depth) #\space))
  (define (indented str [depth 1])
    (if str (format "~a~a" (indent depth) str) #f))
  (define (wrap l r center) (format "~a~a~a" l center r))
  (define (parens x) (wrap "(" ")" x))
  (define (brackets x) (wrap "[" "]" x))
  (define (angles x) (wrap "<" ">" x))
  (define (braces x) (wrap "{" "}" x))
  (define (sep delim parts)
    (string-join (filter (lambda (x) x) parts) delim))
  (define (comma-sep xs) (sep ", " xs))
  (define (spaces . xs) (sep " " xs))
  (define (concat . xs) (sep "" xs))
  (define (newlines . xs) (sep (format "\n~a" (indent ind)) xs))

  (define (pretty-return-ann ann)
    (if (a-blank? ann) #f (format "-> ~a" (pretty-ann ann))))

  (define (pretty-doc doc)
    (if (equal? "" doc) #f (format "doc: \"~a\"" doc)))

  (define (pretty-fun-header name params args ann)
    (concat
     (spaces
      (pretty "fun")
      (if (empty? params) #f (angles (comma-sep (map pretty params))))
      (concat (pretty name)
              (parens (comma-sep (map pretty args))))
      (pretty-return-ann ann))
     (pretty ":")))

  (define (pretty-method-header args ann)
    (concat
     (spaces
      (pretty "method")
      (parens (comma-sep (map pretty args)))
      (pretty-return-ann ann))
     (pretty ":")))

  (define (pretty-check block)
    (if (empty? (s-block-stmts block)) #f
        (newlines "check:"
                  (indented (prettier block)))))

  (match ast

    [(? string? ast) ast]

    [(? symbol? ast) (symbol->string ast)]

    [(s-prog _ imps block)
     (apply newlines (append (map pretty imps) (list (pretty block))))]

    [(s-block _ stmts)
     (apply newlines (map pretty stmts))]

    [(s-let _ bnd val)
     (format "~a = ~a" (pretty bnd) (pretty val))]

    [(s-var _ bnd val)
     (format "var ~a = ~a" (pretty bnd) (pretty val))]

    [(s-bind _ id ann shadow)
     (if (a-blank? ann)
         (pretty id)
         (format "~a~a :: ~a" (if shadow "shadow " "") (pretty id) (pretty-ann ann)))]

    [(s-fun _ name params args ann doc body check)
     (newlines (pretty-fun-header name params args ann)
               (indented (pretty-doc doc))
               (indented (prettier body))
               (pretty-check check)
               "end")]

    [(s-method _ args ann doc body check)
     (define one-line (and (equal? "" doc) (empty? (s-block-stmts check))))
     ((if one-line spaces newlines)
      (pretty-method-header args ann)
      (indented (pretty-doc doc))
      (if one-line (prettier body) (indented (prettier body)))
      (pretty-check check)
      "end")]

    [(s-lam _ params args ann doc body check)
     (define one-line (and (equal? "" doc) (empty? (s-block-stmts check))))
     ((if one-line spaces newlines)
      (pretty-fun-header "" params args ann)
      (indented (pretty-doc doc))
      (if one-line (prettier body) (indented (prettier body)))
      (pretty-check check)
      "end")]

    [(s-assign s name expr)
     (format "~a := ~a" name (pretty expr))]

    [(s-when _ cond consq)
     (newlines (format "when ~a:" (pretty cond))
               (indented (prettier consq)))]

    [(s-if _ (cons (s-if-branch _ cond consq) brs))
     (newlines (format "if ~a:" (pretty cond))
               (indented (prettier consq))
               (if (empty? brs) #f (apply newlines (map pretty brs)))
               "end")]

    [(s-if-else _ (cons (s-if-branch _ cond consq) brs) else)
     (newlines (format "if ~a:" (pretty cond))
               (indented (prettier consq))
               (if (empty? brs) #f (apply newlines (map pretty brs)))
               "else:"
               (indented (prettier else))
               "end")]

    [(s-if-branch _ cond consq)
     (newlines (format "else if ~a:" (pretty cond))
               (indented (prettier consq)))]

    [(s-for _ iter binds ann body)
     (newlines (concat "for "
                       (pretty iter)
                       (parens (comma-sep (map pretty binds)))
                       (pretty-return-ann ann)
                       ":")
               (indented (prettier body)))]

    [(s-for-bind _ bind body)
     (format "~a from ~a" (pretty bind) (pretty body))]

    [(s-list _ elts)
     (brackets (comma-sep (map pretty elts)))]

    [(s-obj _ fields)
     (braces (comma-sep (map pretty fields)))]

    [(s-extend _ expr fields)
     (format "~a.~a" (pretty expr) (braces (comma-sep (map pretty fields))))]

    [(s-data-field _ name value)
     (format "~a : ~a" (pretty name) (pretty value))]

    [(s-datatype _ name params variants check)
     (newlines (format "datatype ~a~a:" name (if (empty? params) ""
                                                 (comma-sep (map symbol->string params))))
               (apply newlines (map pretty variants))
               "end")]

    [(s-datatype-variant _ name binds (s-datatype-constructor _ self body))
     (indented (format "| ~a(~a) with constructor(~a): ~a end"
                       name
                       (comma-sep (map pretty binds))
                       (symbol->string self)
                       (pretty body)))]

    [(s-datatype-singleton-variant _ name (s-datatype-constructor _ self body))
     (indented (format "| ~a with constructor(~a): ~a end"
                       name
                       (symbol->string self)
                       (pretty body)))]

    [(s-cases _ type val branches)
     (newlines (format "cases(~a) ~a:" (pretty-ann type) (pretty val))
               (apply newlines (map pretty branches))
               "end")]

    [(s-cases-else _ type val branches _else)
     (newlines (format "cases(~a) ~a:" (pretty-ann type) (pretty val))
               (apply newlines (map pretty branches))
               (indented (format "| else => ~a" (pretty _else)))
               "end")]

    [(s-cases-branch _ name args body)
     (indented (format "| ~a(~a) => ~a" (symbol->string name)
                        (comma-sep (map (λ (b) (symbol->string (s-bind-id b))) args))
                        (pretty body)))]

    ; TODO: method-field

    [(s-app _ fun args)
     (format "~a(~a)" (pretty fun) (comma-sep (map pretty args)))]

    [(s-left-app _ obj fun args)
     (concat (pretty obj) "^" (pretty fun) (parens (comma-sep (map pretty args))))]

    [(s-extend _ super fields)
     (format "~a.{ ~a }"
             (pretty super)
             (comma-sep (map pretty fields)))]

    [(s-obj _ fields)
     (format "{ ~a }"
             (comma-sep (map pretty fields)))]

    [(s-op _ op e1 e2)
     (format "~a ~a ~a" (pretty e1) (substring (symbol->string op) 2) (pretty e2))]

    [(s-check-test _ op e1 e2)
     (format "~a ~a ~a" (pretty e1) (substring (symbol->string op) 2) (pretty e2))]

    [(s-not _ e)
     (format "not ~a" (pretty e))]

    [(s-dot _ val field)
     (format "~a.~a" (pretty val) field)]

    [(s-bracket _ val field)
     (format "~a.[~a]" (pretty val) (pretty field))]

    [(s-colon _ obj field)
     (format "~a:~a" (pretty obj) field)]

    [(s-colon-bracket _ obj field)
     (format "~a:[~a]" (pretty obj) (pretty field))]

    [(s-num _ n) (number->string n)]
    [(s-bool _ #t) "true"]
    [(s-bool _ #f) "false"]
    [(s-str _ s) (format "\"~a\"" s)]
    [(s-id _ id) (symbol->string id)]

    [(s-paren _ e) (format "(~a)" (pretty e))]

    [else "<unprintable-expr>"]))


(define (pretty-ann ann)
  (match ann
    [(a-name _ id) (symbol->string id)]
    [(a-dot _ obj fld) (string-join (list (symbol->string obj)
                                          "."
                                          (symbol->string fld)) "")]
    [(a-arrow _ t1 t2) (format "(~a -> ~a)" (string-join (map pretty-ann t1) ", ") (pretty-ann t2))]
    [(a-method _ t1 t2) (format "(~a => ~a)" (string-join (map pretty-ann t1) ", ") (pretty-ann t2))]
    [(a-blank) "Any"]
    [(a-any) "Any"]
    [(a-app _ base args) (format "~a<~a>" (pretty-ann base) (string-join (map pretty-ann args) ", "))]
    [(a-pred _ ann expr) (format "~a(~a)" (pretty-ann ann) (pretty expr))]
    [(a-record _ fields) (format "{~a}" (string-join
                                         (map (λ(f) (format "~a: ~a"
                                                            (a-field-name f)
                                                            (pretty-ann (a-field-ann f))))
                                              fields) ", "))]))

(define (pretty ast) (vary-pretty ast 0))
