#lang racket

(require
  rackunit
  rackunit/text-ui
  srfi/13
  (rename-in (only-in racket/string string-replace) [string-replace string-subst])
  "test-utils.rkt"
  "../lang/tokenizer.rkt"
  "../lang/runtime.rkt"
  "../lang/ast.rkt"
  "../lang/pretty.rkt"
  "../lang/desugar.rkt")

(define verbose #f)
(set! verbose #f)

(define round-trip-test #f)
(set! round-trip-test #f)

(define ast-test #t)

(define-syntax check-parse/fail
  (syntax-rules ()
    [(_ str error)
     (begin
       (when verbose
         (printf "Testing: \n~a\n\n" str))
       (check-exn (lambda (e) (and (exn:fail? e)
                                   (string-contains (exn-message e) error)))
                  (lambda () (parse-pyret str))))]))


(define-syntax check/block
  (syntax-rules ()
    [(_ str stmt ...)
     (begin
       (let ([parse-test (format "import ast as A
                             parsed = A.parse(~s, 'parse-tests', {['check']: false})
                             A.is-s_program(parsed.pre-desugar)"
                             str)])
       (when verbose
         (printf "Testing: \n~a\n\n" str)
         (printf "For ast-test: \n~a\n\n" parse-test))
       ;; NOTE(dbp): because we expect there to be whitespace before paren exprs,
       ;; in test context (where there is no #lang), we prepend everything with " "
       (check-match (parse-pyret (string-append " " str))
                    (s-prog _ empty (s-block _ (list stmt ...))))

       ;; Make sure it *can* be parsed by the AST ffi
       ;; NOTE(joe): HACK HACK HACK.  We don't handle escapes well through all this
       ;; round-tripping, so skip tests that contain escaped escapes.
       (when (and ast-test
                  (not (or (string-contains str "\\n")
                           (string-contains str "\\t")
                           (string-contains str "\\r"))))
         (check-pyret parse-test (p:mk-bool #t)))

       (when round-trip-test
           (check-not-exn
            (lambda ()
              (define pr (pretty (desugar-pyret (parse-pyret str))))
              (when verbose
                (printf "Printed to: \n~a\n\n" pr))
              (parse-pyret pr))))))]))

(define-syntax check-tokenize-exn
  (syntax-rules ()
    [(_ name str pred)
     (let ()
       (define tokenizer (tokenize (open-input-string str) name))
       (check-exn pred (lambda ()
        (define (get-tokens)
          (define next (tokenizer))
          (if (void? next)
              #t
              (get-tokens)))
        (get-tokens)) str))]))

(define tokenizer (test-suite "tokenizer"
  (check-tokenize-exn "plus-no-space" "5+4"
    (lambda (e)
      (and
        (exn:fail:read:pyret:binop? e)
        (string=? (exn:fail:read:pyret-lexeme e) "+"))))
  (check-tokenize-exn "plus-no-space-after" "5 +4"
    (lambda (e)
      (and
        (exn:fail:read:pyret:binop? e)
        (string=? (exn:fail:read:pyret-lexeme e) "+"))))
))

(define literals (test-suite "literals"
  (check/block "'str'" (s-str _ "str"))
  (check/block "'multi
line string'" (s-str _ "multi\nline string"))
  (check/block "\"multi
line string\"" (s-str _ "multi\nline string"))
  (check/block "\"\\\\\" + \"another str\""
    (s-op _ 'op+ (s-str _ "\\") (s-str _ "another str")))
  (check/block "5" (s-num _ 5))
  (check/block "-7" (s-num _ -7))
  (check/block "10.2" (s-num _ 102/10))
  (check/block "-10.2" (s-num _ -102/10))

  (check/block "true" (s-bool _ #t))
  (check/block "false" (s-bool _ #f))

  (check/block "8 'foo'" (s-num _ 8) (s-str _ "foo"))

  (check/block "{}" (s-obj _ empty))
  (check/block "{x:9}" (s-obj _ (list (s-data-field _ (s-str _ "x") (s-num _ 9)))))
  (check/block "{x:6,y:'foo'}" (s-obj _ (list (s-data-field _ (s-str _ "x") (s-num _ 6))
                                                              (s-data-field _ (s-str _ "y") (s-str _ "foo")))))
  (check/block "{x:{}}" (s-obj _ (list (s-data-field _ (s-str _ "x") (s-obj _ empty)))))

  (check/block "{[xfield]:3}" (s-obj _ (list (s-data-field _ (s-id _ 'xfield) (s-num _ 3)))))
  (check/block "{['x']:2}" (s-obj _ (list (s-data-field _ (s-str _ "x") (s-num _ 2)))))

  (check/block "{x:idfield}" (s-obj _ (list (s-data-field _ (s-str _ "x") (s-id _ 'idfield)))))

  (check/block "[]" (s-list _ empty))
  (check/block "[5]" (s-list _ (list (s-num _ 5))))

  ;; NOTE(dbp): we are writing a pyret program in a racket string, so to replicate
  ;; the literal "\" "n" typed by the racket programmer, we need to escape.
  ;; What we should get back is the actual escape characters.
  (check/block "'string\\nwith\\r\\nspecial\\tcharacters'"
               (s-str _ "string\nwith\r\nspecial\tcharacters"))

  (check/block "\"\\\"\"" (s-str _ "\""))
  (check/block "\"here come some string escapes: \\\"\\\"\\\" there they are\""
    (s-str _ "here come some string escapes: \"\"\" there they are"))
  (check/block "\"here come some string escapes: \\\'\\\'\\\' there they are\""
    (s-str _ "here come some string escapes: \'\'\' there they are"))
  (check/block "\"here come some stilted string escapes: \\\\\\\"\\\\\\\'\\\\\\\" there they are\""
    (s-str _ "here come some stilted string escapes: \\\"\\\'\\\" there they are"))
  (check/block "'\\''" (s-str _ "'"))

  (check-parse/fail "\"str\\\"" "unexpected")
))

(define methods (test-suite "methods"
  (check/block "{f(): x end}"
               (s-obj _ (list (s-method-field _ (s-str _ "f") (list) (a-blank) _ (s-block _ (list (s-id _ 'x))) _))))
  (check/block "{f(): x end}"
               (s-obj _ (list (s-method-field _ (s-str _ "f") (list) (a-blank) _ (s-block _ (list (s-id _ 'x))) _))))

  (check/block "{[f](): x end}"
               (s-obj _ (list (s-method-field _ (s-id _ 'f)
                                              (list) (a-blank) _ (s-block _ (list (s-id _ 'x))) _))))
  (check/block "{['f'](): x end}"
               (s-obj _ (list (s-method-field _ (s-str _ "f")
                                              (list) (a-blank) _ (s-block _ (list (s-id _ 'x))) _))))

  (check/block "method(x,y): 1 end"
               (s-method _ (list (s-bind _ #f 'x (a-blank)) (s-bind _ #f 'y (a-blank)))
                         (a-blank) _ (s-block _ (list (s-num _ 1))) _))

  (check/block "method(self): 1 where: foo end"
               (s-method _ (list (s-bind _ #f 'self (a-blank)))
                         (a-blank) _ (s-block _ (list (s-num _ 1)))
                         (s-block _ (list (s-id _ 'foo)))))

  (check/block "method(self): 1 where: end"
               (s-method _ (list (s-bind _ #f 'self (a-blank)))
                         (a-blank) _ (s-block _ (list (s-num _ 1)))
                         (s-block _ empty)))

  (check/block "{f(): x where: 1 end}"
               (s-obj _ (list (s-method-field _ (s-str _ "f") (list) (a-blank) _
                                              (s-block _ (list (s-id _ 'x)))
                                              (s-block _ (list (s-num _ 1)))))))

  (check/block "method(self): doc: 'hello' 1 end"
               (s-method _ (list (s-bind _ #f 'self (a-blank)))
                         (a-blank) "hello" (s-block _ (list (s-num _ 1)))
                         (s-block _ empty)))

  (check/block "{f(): doc: 'hello' 1 end}"
               (s-obj _ (list (s-method-field _ (s-str _ "f") (list) (a-blank) "hello"
                                              (s-block _ (list (s-num _ 1)))
                                              (s-block _ empty)))))
))

(define functions (test-suite "functions"
  (check/block "f()" (s-app _ (s-id _ 'f) empty))
  (check/block "f(5)" (s-app _ (s-id _ 'f) (list (s-num _ 5))))

  (check/block "f(5,'foo')" (s-app _ (s-id _ 'f) (list (s-num _ 5) (s-str _ "foo"))))

  (check/block "fun f(): 5 end"
              (s-fun _ 'f empty empty (a-blank) _ (s-block _ (list (s-num _ 5)))
                     (s-block _ empty)))

  (check/block "fun g(g): 5 end"
              (s-fun _ 'g empty (list (s-bind _ #f 'g (a-blank))) (a-blank)
                     _
                     (s-block _ (list (s-num _ 5)))
                     (s-block _ empty)))

  (check/block "fun g(g,f,x): 5 end"
               (s-fun _ 'g empty (list (s-bind _ #f 'g (a-blank))
                                       (s-bind _ #f 'f (a-blank))
                                       (s-bind _ #f 'x (a-blank))) (a-blank)
                      _
                      (s-block _ (list (s-num _ 5)))
                      (s-block _ empty)))

  (check/block "brander()"
               (s-app _ (s-id _ 'brander) (list)))

  (check/block "fun f(): 5 where: 4 end"
               (s-fun _ 'f empty empty (a-blank) _
                      (s-block _ (list (s-num _ 5)))
                      (s-block _ (list (s-num _ 4)))))

  (check/block "fun f(): 5 where: end"
               (s-fun _ 'f empty empty (a-blank) _
                      (s-block _ (list (s-num _ 5)))
                      (s-block _ empty)))
))

(define graph (test-suite "graph"

  (check/block "graph:
                  x = m.constr(1, y)
                  y = m.other-constr(x)
                end"
                (s-graph _
                  (list (s-let _ (s-bind _ #f 'x (a-blank))
                                 (s-app _ (s-dot _ (s-id _ 'm) 'constr)
                                        (list
                                         (s-num _ 1)
                                         (s-id _ 'y))))
                        (s-let _ (s-bind _ #f 'y (a-blank))
                                 (s-app _ (s-dot _ (s-id _ 'm) 'other-constr)
                                        (list
                                          (s-id _ 'x)))))))
  (check/block "graph:
                  BOS = mlink(PVD, mlink(WOR, mempty))
                  WOR = mlink(BOS, mempty)
                  PVD = mlink(BOS, mempty)
                end"
                (s-graph _
                  (list (s-let _ (s-bind _ #f 'BOS (a-blank))
                                 (s-app _ (s-id _ 'mlink)
                                        (list
                                          (s-id _ 'PVD)
                                          (s-app _ (s-id _ 'mlink)
                                            (list (s-id _ 'WOR) (s-id s 'mempty))))))
                        (s-let _ (s-bind _ #f 'WOR (a-blank))
                                 (s-app _ (s-id _ 'mlink)
                                        (list
                                          (s-id _ 'BOS)
                                          (s-id _ 'mempty))))
                        (s-let _ (s-bind _ #f 'PVD (a-blank))
                                 (s-app _ (s-id _ 'mlink)
                                        (list
                                          (s-id _ 'BOS)
                                          (s-id _ 'mempty)))))))

                ))

(define user-block (test-suite "user-block"

  (check/block "f = block: nothing end"
    (s-let _ (s-bind _ #f 'f (a-blank))
     (s-user-block _ (s-block _ (list (s-id _ 'nothing))))))

  (check/block "f = block:
      x = block: 5 end
      nothing
    end"
    (s-let _ (s-bind _ #f 'f (a-blank))
      (s-user-block _
        (s-block _ (list
          (s-let _ (s-bind _ #f 'x (a-blank))
            (s-user-block _ (s-block _ (list (s-num _ 5)))))
          (s-id _ 'nothing))))))

))

#;(define let-block (test-suite "let-block"

  (check/block "let: x end"
    (s-let-block _ (list)
      (s-block _ (list (s-id _ 'x)))))


  (check/block "let x = 10: x end"
    (s-let-block _ (list
                    (s-let _ (s-bind _ #f 'x (a-blank)) (s-num 10))))
      (s-block _ (list (s-id _ 'x))))

  (check/block "let var x = 10: x end"
    (s-let-block _ (list
                    (s-var _ (s-bind _ #f 'x (a-blank)) (s-num 10))))
      (s-block _ (list (s-id _ 'x))))

  (check/block "let var x = 10, y = 5: x end"
    (s-let-block _ (list
                    (s-var _ (s-bind _ #f 'x (a-blank)) (s-num 10))
                    (s-let _ (s-bind _ #f 'y (a-blank)) (s-num 5))))
      (s-block _ (list (s-id _ 'x))))

  (check/block "let var x = 10, var y = 5: x end"
    (s-let-block _ (list
                    (s-var _ (s-bind _ #f 'x (a-blank)) (s-num 10))
                    (s-var _ (s-bind _ #f 'y (a-blank)) (s-num 5))))
      (s-block _ (list (s-id _ 'x))))

  (check/block "let x = 10, var y = 5: x end"
    (s-let-block _ (list
                    (s-let _ (s-bind _ #f 'x (a-blank)) (s-num 10))
                    (s-var _ (s-bind _ #f 'y (a-blank)) (s-num 5))))
      (s-block _ (list (s-id _ 'x))))

  (check/block "let x = 10, y = 5, z = 52: x end"
    (s-let-block _ (list
                    (s-let _ (s-bind _ #f 'x (a-blank)) (s-num 10))
                    (s-let _ (s-bind _ #f 'y (a-blank)) (s-num 5))
                    (s-let _ (s-bind _ #f 'z (a-blank)) (s-num 52))))
      (s-block _ (list (s-id _ 'x))))

))

(define fields (test-suite "fields"

  (check/block "o.x" (s-dot _ (s-id _ 'o) 'x))
  (check/block "seal({x:5}, []).x"
               (s-dot _ (s-app _ (s-id _ 'seal) (list (s-obj _ (list (s-data-field _ (s-str _ "x") (s-num _ 5))))
                                                      (s-list _ empty)))
                      'x))

  (check/block "o.['x']" (s-bracket _ (s-id _ 'o) (s-str _ "x")))
  (check/block "o.[x]" (s-bracket _ (s-id _ 'o) (s-id _ 'x)))

  (check/block "3.add" (s-dot _ (s-num _ 3) 'add))

  (check/block "{mutable x: 5}" (s-obj _ (list (s-mutable-field _ (s-str _ "x") (a-blank) (s-num _ 5)))))
  (check/block "{mutable x :: Number: 5}" (s-obj _ (list (s-mutable-field _ (s-str _ "x") (a-name _ 'Number) (s-num _ 5)))))
  (check/block "{mutable x: 5, y: 10}"
    (s-obj _ (list (s-mutable-field _ (s-str _ "x") (a-blank) (s-num _ 5)) (s-data-field _ (s-str _ "y") (s-num _ 10)))))

  (check/block "o!f" (s-get-bang _ (s-id _ 'o) 'f))

  (check/block "o!f!g" (s-get-bang _ (s-get-bang _ (s-id _ 'o) 'f) 'g))
  (check/block "o!f.g" (s-dot _ (s-get-bang _ (s-id _ 'o) 'f) 'g))
  (check/block "o!f()" (s-app _ (s-get-bang _ (s-id _ 'o) 'f) (list)))

  (check/block "{x:5}.{y:3}"
               (s-extend _
                        (s-obj _ (list (s-data-field _ (s-str _ "x") (s-num _ 5))))
                        (list (s-data-field _ (s-str _ "y") (s-num _ 3)))))

  (check/block "5.{foo: 12}"
               (s-extend _
                        (s-num _ 5)
                        (list (s-data-field _ (s-str _ "foo") (s-num _ 12)))))

  (check/block "List.{length: 0, width: 0}"
               (s-extend _
                        (s-id _ 'List)
                        (list (s-data-field _ (s-str _ "length") (s-num _ 0))
                              (s-data-field _ (s-str _ "width") (s-num _ 0)))))

  (check/block "5!{x: 5}"
               (s-update _
                         (s-num _ 5)
                         (list (s-data-field _ (s-str _ "x") (s-num _ 5)))))

  (check/block "5!{x: 5, y: 10}"
               (s-update _
                         (s-num _ 5)
                         (list (s-data-field _ (s-str _ "x") (s-num _ 5))
                               (s-data-field _ (s-str _ "y") (s-num _ 10)))))

  (check/block "o:f"
               (s-colon _
                        (s-id _ 'o)
                        'f))

  (check/block "{f:4}:f"
               (s-colon _
                        (s-obj _ (list (s-data-field _ (s-str _ "f") (s-num _ 4))))
                        'f))

  (check/block "methods:[name]._fun()(inst-with-super, arg)"
               (s-app _
                (s-app _
                  (s-dot _
                    (s-colon-bracket _
                      (s-id _ 'methods)
                      (s-id _ 'name))
                    '_fun)
                  (list))
                (list (s-id _ 'inst-with-super) (s-id _ 'arg))))

  (check/block
   "o.{x : 5}"
   (s-extend _ (s-id _ 'o) (list (s-data-field _ (s-str _ "x") (s-num _ 5)))))

  (check/block
   "{x : 5}.{x : 10, y : 6}"
   (s-extend _ (s-obj _ (list (s-data-field _ (s-str _ "x") (s-num _ 5))))
            (list (s-data-field _ (s-str _ "x") (s-num _ 10))
                  (s-data-field _ (s-str _ "y") (s-num _ 6)))))

))


(define annotations (test-suite "annotations"
  (check/block
   "fun <a> f(x :: a) -> a: x end"
   (s-fun _ 'f (list 'a) (list (s-bind _ #f 'x (a-name _ 'a))) (a-name _ 'a)
    _
    (s-block _ (list (s-id _ 'x)))
    (s-block _ empty)))

  (check/block
   "fun <a,b> f(x :: a) -> b: x end"
   (s-fun _ 'f (list 'a 'b) (list (s-bind _ #f 'x (a-name _ 'a))) (a-name _ 'b)
    _
    (s-block _ (list (s-id _ 'x)))
    (s-block _ empty)))


  (check/block
   "fun <a,b> f(x :: a) -> b: doc: 'some documentation' x end"
   (s-fun _ 'f (list 'a 'b) (list (s-bind _ #f 'x (a-name _ 'a))) (a-name _ 'b)
    "some documentation"
    (s-block _ (list (s-id _ 'x)))
    (s-block _ empty)))


  (check/block "fun foo(x) -> (Number -> Number): 'should return a function from num to num' end"
               (s-fun _ 'foo empty (list (s-bind _ #f 'x (a-blank))) (a-arrow _ (list (a-name _ 'Number)) (a-name _ 'Number))
                      _
                      (s-block _ (list (s-str _ _)))
                      (s-block _ empty)))
  (check/block "fun foo(x :: Bool) -> Bool: x end"
               (s-fun _ 'foo empty (list (s-bind _ #f 'x (a-name _ 'Bool))) (a-name _ 'Bool)
                      _
                      (s-block _ (list (s-id _ 'x)))
                      (s-block _ empty)))

  (check/block "var x :: Number = 5" (s-var _ (s-bind _ #f 'x (a-name _ 'Number))
                                             (s-num _ 5)))
  (check/block "var x :: Number = 'hello'" (s-var _ (s-bind _ #f 'x (a-name _ 'Number))
                                           (s-str _ "hello")))

  (check/block "var f :: (Number, Number -> Number) = plus"
         (s-var _ (s-bind _ #f 'f (a-arrow _
                (list (a-name _ 'Number) (a-name _ 'Number))
                (a-name _ 'Number)))
          (s-id _ 'plus)))


  (check/block "var x :: {} = 4" (s-var _ (s-bind _ #f 'x (a-record _ (list))) (s-num _ 4)))
  (check/block "var x :: {foo: Number} = 4"
               (s-var _ (s-bind _ #f 'x (a-record _ (list (a-field _ "foo" Number)))) (s-num _ 4)))
  (check/block "var x :: {foo: Number} = 4"
               (s-var _ (s-bind _ #f 'x (a-record _ (list (a-field _ "foo" Number)))) (s-num _ 4)))
  (check/block "var x :: {foo: Number, a: Bool} = 4"
               (s-var _ (s-bind _ #f 'x (a-record _ (list (a-field _ "foo" (a-name _ 'Number))
                                                       (a-field _ "a" (a-name _ 'Bool)))))
                      (s-num _ 4)))
  (check/block "var x :: list.List = 4"
               (s-var _ (s-bind _ #f 'x (a-dot _ 'list 'List))
                      (s-num _ 4)))

  (check/block "var x :: list.List<A> = 4"
               (s-var _ (s-bind _ #f 'x (a-app _ (a-dot _ 'list 'List)
                                            (list (a-name _ 'A))))
                      (s-num _ 4)))

  (check/block "var x :: ( -> Number) = 4"
               (s-var _ (s-bind _ #f 'x (a-arrow _ empty (a-name _ 'Number)))
                      (s-num _ 4)))

  (check/block "var x :: (Number -> Number) = 4"
               (s-var _ (s-bind _ #f 'x (a-arrow _ (list (a-name _ 'Number)) (a-name _ 'Number)))
                      (s-num _ 4)))

  (check/block "x :: Any = 4"
               (s-let _ (s-bind _ #f 'x (a-any))
                      (s-num _ 4)))

  (check/block "foo<Number>(a)"
               (s-app _ (s-instantiate _ (s-id _ 'foo) (list (a-name _ 'Number))) (list (s-id _ 'a))))
  (check/block "foo<Number, String>(a)"
               (s-app _ (s-instantiate _ (s-id _ 'foo)
                                       (list (a-name _ 'Number) (a-name _ 'String)))
                      (list (s-id _ 'a))))
  (check/block "foo<{x :: Number}>(a)"
               (s-app _ (s-instantiate _ (s-id _ 'foo)
                                       (list (a-record _ (list (a-field _ "x" (a-name _ 'Number))))))
                      (list (s-id _ 'a))))

  (check/block "foo<Option<String>>(a)"
               (s-app _ (s-instantiate _ (s-id _ 'foo)
                                       (list (a-app _ (a-name _ 'Option) (list (a-name _ 'String)))))
                      (list (s-id _ 'a))))

  (check/block "foo<Number(positive)>(a)"
               (s-app _ (s-instantiate _ (s-id _ 'foo)
                                       (list (a-pred _ (a-name _ 'Number) (s-id _ 'positive))))
                      (list (s-id _ 'a))))

  (check/block "foo<list.List>(a)"
               (s-app _ (s-instantiate _ (s-id _ 'foo)
                                       (list (a-dot _ 'list 'List)))
                      (list (s-id _ 'a))))

  (check/block "foo<(Number -> String), Foo>(a)"
               (s-app _ (s-instantiate _ (s-id _ 'foo)
                                       (list (a-arrow _ (list (a-name _ 'Number)) (a-name _ 'String))
                                             (a-name _ 'Foo)))
                      (list (s-id _ 'a))))

  (check/block "foo<Number>"
               (s-instantiate _ (s-id _ 'foo) (list (a-name _ 'Number))))

  (check/block "foo(bar)<Baz>"
               (s-instantiate _ (s-app _ (s-id _ 'foo) (list (s-id _ 'bar)))
                              (list (a-name _ 'Baz))))

))

(define anon-func (test-suite "anon-func"
  (check/block "fun: x end"
               (s-lam _ empty (list)
                      (a-blank)
                      _
                      (s-block _ (list (s-id _ 'x)))
                      _))

  (check/block "fun -> Number: x end"
               (s-lam _ empty (list)
                      (a-name _ 'Number)
                      _
                      (s-block _ (list (s-id _ 'x)))
                      _))

  (check/block "fun(x :: Number, y :: Bool) -> Number: x.send(y) end"
               (s-lam _ empty (list (s-bind _ #f 'x (a-name _ 'Number))
                              (s-bind _ #f 'y (a-name _ 'Bool)))
                      (a-name _ 'Number)
                      _
                      (s-block _ (list (s-app _
                                              (s-dot _ (s-id _ 'x) 'send)
                                              (list (s-id _ 'y)))))
                      _))

  (check/block "fun(x,y,z): x end"
               (s-lam _ empty (list (s-bind _ #f 'x (a-blank))
                              (s-bind _ #f 'y (a-blank))
                              (s-bind _ #f 'z (a-blank)))
                      (a-blank)
                      _
                      (s-block _ (list (s-id _ 'x)))
                      _))

  (check/block "fun(x): x where: foo end"
               (s-lam _ empty (list (s-bind _ #f 'x (a-blank)))
                      (a-blank)
                      _
                      (s-block _ (list (s-id _ 'x)))
                      (s-block _ (list (s-id _ 'foo)))))



(check/block "fun (x,y,z): x end"
               (s-lam _ empty (list (s-bind _ #f 'x (a-blank))
                              (s-bind _ #f 'y (a-blank))
                              (s-bind _ #f 'z (a-blank)))
                      (a-blank)
                      _
                      (s-block _ (list (s-id _ 'x)))
                      _))

(check/block "fun (): x end"
               (s-lam _ empty (list)
                      (a-blank)
                      _
                      (s-block _ (list (s-id _ 'x)))
                      _))

  ;; non-empty lists for x
  (check/block "var x :: List(list.is-cons) = 4"
    (s-var _
           (s-bind _ #f
                     'x
                     (a-pred _
                             (a-name _ 'List)
                             (s-dot _ (s-id _ 'list)
                                      'is-cons)))
           (s-num _ 4)))

  ;; non-empty lists of strings for x
  (check/block "var x :: List<String>(list.is-cons) = 4"
    (s-var _
           (s-bind _ #f
                     'x
                     (a-pred _
                             (a-app _ (a-name _ 'List)
                                      (list (a-name _ 'String)))
                             (s-dot _ (s-id _ 'list)
                                      'is-cons)))
           (s-num _ 4)))
))

(define cases (test-suite "cases"

  (check/block "if x:o:f() end"
               (s-if _ (list
                        (s-if-branch
                         _
                         (s-colon _ (s-id _ 'x) 'o)
                         (s-block _ (list (s-app _ (s-id _ 'f) (list))))))))

  (check/block "if false: 5 else: 42 end"
               (s-if-else _ (list (s-if-branch _ (s-bool _ #f) (s-block _ (list (s-num _ 5)))))
                            (s-block _ (list (s-num _ 42)))))

  (check/block "if false: 5 else if true: 24 end"
               (s-if _ (list (s-if-branch _ (s-bool _ #f) (s-block _ (list (s-num _ 5))))
                             (s-if-branch _ (s-bool _ #t) (s-block _ (list (s-num _ 24)))))))

  (check/block "if false: 5 else if true: 24 else: 6 end"
               (s-if-else _ (list (s-if-branch _ (s-bool _ #f) (s-block _ (list (s-num _ 5))))
                                  (s-if-branch _ (s-bool _ #t) (s-block _ (list (s-num _ 24)))))
                          (s-block _ (list (s-num _ 6)))))

  (check/block "if 0 < 1: 5 else if 1 < 0: 24 else: 6 end"
               (s-if-else _ (list (s-if-branch _ (s-op _ 'op< (s-num _ 0) (s-num _ 1))
                                                 (s-block _ (list (s-num _ 5))))
                                  (s-if-branch _ (s-op _ 'op< (s-num _ 1) (s-num _ 0)) (s-block _ (list (s-num _ 24)))))
                          (s-block _ (list (s-num _ 6)))))

  (check/block "cases(List) 5: | empty() => 1 end"
               (s-cases _ (a-name _ 'List) (s-num _ 5)
                  (list
                    (s-cases-branch _ 'empty empty (s-block _ (list (s-num _ 1)))))))
  (check/block "cases(List<a>) 5: | empty() => 1 end"
               (s-cases _ (a-app _ (a-name _ 'List) (list (a-name _ 'a))) (s-num _ 5)
                  (list
                    (s-cases-branch _ 'empty empty (s-block _ (list (s-num _ 1)))))))

  (check/block "when true: 5 end"
    (s-when _ (s-bool _ #t) (s-block _ (list (s-num _ 5)))))

  (check/block "when x.equals(42): print('got it') print('the answer') end"
    (s-when _ (s-app _ (s-dot _ (s-id _ 'x) 'equals) (list (s-num _ 42)))
      (s-block _ (list
        (s-app _ (s-id _ 'print) (list (s-str _ "got it")))
        (s-app _ (s-id _ 'print) (list (s-str _ "the answer")))))))

))

(define data (test-suite "data"

  (check/block "data Foo: end"
               (s-data _ 'Foo empty empty (list) (list) (s-block _ _)))
  (check/block "data Foo: where: end"
               (s-data _ 'Foo empty empty (list) (list) (s-block _ _)))

  (check/block "  data Foo: | bar() end"
               (s-data _ 'Foo empty empty (list (s-variant _ 'bar (list) (list))) (list) (s-block _ _)))

  (check/block "data NumList:
      | empty()
      | cons(first :: Number, rest :: NumList)
    end"
    (s-data _ 'NumList empty (list) (list
      (s-variant _ 'empty (list) (list))
      (s-variant _ 'cons (list (s-variant-member _ 'normal (s-bind _ #f 'first (a-name _ 'Number)))
                               (s-variant-member _ 'normal (s-bind _ #f 'rest (a-name _ 'NumList))))
                 (list)))
           (list) (s-block _ _)))
  (check/block "data List<a>: | empty() end" (s-data _ 'List (list 'a) empty (list (s-variant _ 'empty (list) (list))) (list) (s-block _ _)))

  (check/block
   "data List<a>: | cons(field, l :: List<a>) end"
   (s-data _ 'List (list 'a) empty
           (list (s-variant
                  _
                  'cons
                  (list (s-variant-member _ 'normal (s-bind _ #f 'field (a-blank)))
                        (s-variant-member _ 'normal (s-bind _ #f 'l (a-app _ (a-name _ 'List)
                                            (list (a-name _ 'a))))))
                  (list))) (list)
                  (s-block _ _)))

  (check/block
   "data Der deriving Eq, Show(true): | derCase1 end"
   (s-data _ 'Der empty
           (list (s-id _ 'Eq)
                 (s-app _ (s-id _ 'Show) (list (s-bool _ #t))))
           (list (s-singleton-variant
                  _
                  'derCase1
                  (list)))
           (list)
           (s-block _ _)))

  (check/block
   "data Mutable:
      | v1(mutable x :: String)
      | v2(x, mutable y)
    end"
    (s-data _ 'Mutable (list) (list)
      (list
        (s-variant _ 'v1 (list (s-variant-member _ 'mutable (s-bind _ #f 'x (a-name _ 'String)))) (list))
        (s-variant _ 'v2 (list
          (s-variant-member _ 'normal (s-bind _ #f 'x (a-blank)))
          (s-variant-member _ 'mutable (s-bind _ #f 'y (a-blank)))) (list)))
      (list)
      (s-block _ _)))

  (check/block
   "data List: | empty end"
   (s-data _ 'List (list) empty
    (list (s-singleton-variant
           _
           'empty
           (list)))
    (list)
    (s-block _ _)))

  (check/block
   "data List: | empty with: length(self): 0 end end"
   (s-data _ 'List (list) empty
    (list (s-singleton-variant
           _
           'empty
           (list (s-method-field _ (s-str _ "length") (list (s-bind _ #f 'self (a-blank))) (a-blank) _ (s-block _ (list (s-num _ 0))) _))))
    (list)
    (s-block _ _)))

  (check/block
   "data Foo: | bar() with: x(self): self end end"
   (s-data _ 'Foo (list) empty
           (list (s-variant _ 'bar (list)
                            (list (s-method-field _
                                                  (s-str _ "x")
                                                  (list (s-bind _ #f 'self (a-blank)))
                                                  (a-blank)
                                                  _
                                                  (s-block _ (list (s-id _ 'self)))
                                                  _))))
           (list)
           (s-block _ _)))

  (check/block
   "data Foo: | bar() with: x(self) -> Num: self end
    sharing:
      z: 10
    end"
   (s-data _ 'Foo (list) empty (list (s-variant _
                                          'bar
                                          (list)
                                          (list (s-method-field _
                                                                (s-str _ "x")
                                                                (list (s-bind _ #f 'self (a-blank)))
                                                                (a-name _ 'Num)
                                                                _
                                                                (s-block _ (list (s-id _ 'self)))
                                                                _))))
           (list (s-data-field _ (s-str _ "z") (s-num _ 10)))
           (s-block _ _)))

   (check/block
    "data Foo: | bar() where: 5 end"
    (s-data _ 'Foo empty empty
            (list (s-variant _ 'bar (list) (list)))
            (list)
            (s-block _ (list (s-num _ 5)))))

     (check/block
      "data Foo: | bar() | baz() sharing: z: 10 where: end"
      (s-data _ 'Foo empty empty
              (list (s-variant _ 'bar (list) (list))
                    (s-variant _ 'baz (list) (list)))
              (list (s-data-field _ (s-str _ "z") (s-num _ 10)))
              (s-block _ (list))))

   (check/block
    "data F:
      | v(cyclic x :: Number)
     end"
    (s-data _ 'F empty empty
      (list (s-variant _ 'v (list (s-variant-member _ 'cyclic (s-bind _ #f 'x (a-name _ 'Number)))) (list)))
      (list)
      (s-block _ (list))))

   (check/block
    "datatype Foo:
      | foo() with constructor(self): self end
     end"
    (s-datatype _ 'Foo empty (list (s-datatype-variant _ 'foo empty
                                                       (s-datatype-constructor
                                                        _ 'self
                                                        (s-block _ (list (s-id _ 'self))))))
                (s-block _ (list))))

   (check/block
    "datatype Foo:
      | foo() with constructor(self): self end
      | bar(a) with constructor(self): self end
     end"
    (s-datatype _ 'Foo empty (list (s-datatype-variant _ 'foo empty
                                                       (s-datatype-constructor
                                                        _ 'self
                                                        (s-block _ (list (s-id _ 'self)))))
                                   (s-datatype-variant _ 'bar (list (s-variant-member
                                                                     _ 'normal
                                                                     (s-bind _ #f 'a (a-blank))))
                                                       (s-datatype-constructor
                                                        _ 'self
                                                        (s-block _ (list (s-id _ 'self))))))
                (s-block _ (list))))

   (check/block
    "datatype Foo:
      | foo(a :: Number, b :: Foo) with constructor(self): self end
      | bar with constructor(self): self end
     end"
    (s-datatype _ 'Foo empty (list (s-datatype-variant _ 'foo (list (s-variant-member
                                                                     _ 'normal
                                                                     (s-bind _ #f 'a (a-name _ 'Number)))
                                                                    (s-variant-member
                                                                     _ 'normal
                                                                     (s-bind _ #f 'b (a-name _ 'Foo))))
                                                       (s-datatype-constructor
                                                        _ 'self
                                                        (s-block _ (list (s-id _ 'self)))))
                                   (s-datatype-singleton-variant _ 'bar
                                                                 (s-datatype-constructor
                                                        _ 'self
                                                        (s-block _ (list (s-id _ 'self))))))
                (s-block _ (list))))

   (check/block
    "datatype Foo<T>:
      | foo(a :: T) with constructor(self): self end
     end"
    (s-datatype _ 'Foo (list 'T) (list (s-datatype-variant _ 'foo (list (s-variant-member
                                                                         _ 'normal
                                                                         (s-bind _ #f 'a (a-name _ 'T))))
                                                       (s-datatype-constructor
                                                        _ 'self
                                                        (s-block _ (list (s-id _ 'self))))))
                (s-block _ (list))))

   (check/block
    "datatype Foo:
      | foo(a :: Number, b :: Foo) with constructor(self): self end
      | bar() with constructor(self): self end
     where:
       bar()
     end"
    (s-datatype _ 'Foo empty (list (s-datatype-variant _ 'foo (list (s-variant-member
                                                                     _ 'normal
                                                                     (s-bind _ #f 'a (a-name _ 'Number)))
                                                                    (s-variant-member
                                                                     _ 'normal
                                                                     (s-bind _ #f 'b (a-name _ 'Foo))))
                                                       (s-datatype-constructor
                                                        _ 'self
                                                        (s-block _ (list (s-id _ 'self)))))
                                   (s-datatype-variant _ 'bar empty
                                                       (s-datatype-constructor
                                                        _ 'self
                                                        (s-block _ (list (s-id _ 'self))))))
                (s-block _ (list (s-app _ (s-id _ 'bar) empty)))))
))

(define for (test-suite "for"

  (check/block
    "for map(elt from lst): elt.plus(42) end"
    (s-for _
           (s-id _ 'map)
           (list (s-for-bind _ (s-bind _ #f elt (a-blank)) (s-id _ 'lst)))
           (a-blank)
           (s-block _ (list
                        (s-app _
                               (s-dot _ (s-id _ 'elt) 'plus)
                               (list (s-num _ 42)))))))

  (check/block
    "for get.iterator('from-expr')() -> String: 'body' end"
    (s-for _
           (s-app _ (s-dot _ (s-id _ 'get) 'iterator) (list (s-str _ "from-expr")))
           (list)
           (a-name _ 'String)
           (s-block _ (list (s-str _ "body")))))

  (check/block
    "for fold(acc :: Number from 0, elt :: Number from [1,2,3]): acc.plus(elt) end"
    (s-for _
           (s-id _ 'fold)
           (list
            (s-for-bind _ (s-bind _ #f acc (a-name _ 'Number))
                          (s-num _ 0))
            (s-for-bind _ (s-bind _ #f elt (a-name _ 'Number))
                          (s-list _ (list (s-num _ 1) (s-num _ 2) (s-num _ 3)))))
           (a-blank)
           (s-block _ (list
             (s-app _ (s-dot _ (s-id _ 'acc) 'plus) (list (s-id _ 'elt)))))))

))


(define modules (test-suite "modules"

  (check-match (parse-pyret "import 'file.arr' as file")
   (s-prog _ (list (s-import _ "file.arr" 'file)) (s-block _ (list))))

  (check-match (parse-pyret "provide {a: 1} end")
   (s-prog _ (list (s-provide _ (s-obj _ (list (s-data-field _ (s-str _ "a") (s-num _ 1))))))
       (s-block _ (list))))

  (check-match (parse-pyret "provide * ")
    (s-prog _ (list (s-provide-all _)) (s-block _ (list))))
))

(define caret (test-suite "caret"
  (check/block "o^f()"
   (s-left-app _ (s-id _ 'o) (s-id _ 'f) (list)))

  (check/block "o^f.g()()"
   (s-app _ (s-left-app _ (s-id _ 'o) (s-dot _ (s-id _ 'f) 'g) (list))
            (list)))

  (check/block "o^f()^g()"
   (s-left-app _
               (s-left-app _ (s-id _ 'o) (s-id _ 'f) (list))
               (s-id _ 'g)
               (list)))

  (check-pyret-exn "o^o2.f.g()" "parsing error")
))


(define exceptions (test-suite "exceptions"
  (check/block "try: a b c except(e): 5 end"
    (s-try _
      (s-block _ (list (s-id _ 'a)
                       (s-id _ 'b)
                       (s-id _ 'c)))
      (s-bind _ #f 'e (a-blank))
      (s-block _ (list (s-num _ 5)))))

  (check/block "try: try: a except(e): 5 end b c except(e): 5 end"
    (s-try _
      (s-block _ (list
        (s-try _ (s-block _ (list (s-id _ 'a)))
                 (s-bind _ #f 'e (a-blank))
                 (s-block _ (list (s-num _ 5))))
        (s-id _ 'b)
        (s-id _ 'c)))
      (s-bind _ #f 'e (a-blank))
      (s-block _ (list (s-num _ 5)))))

  ;; try/except should be an expression
  (check/block "x = try: a except(e): 5 end"
        (s-let _ (s-bind _ #f 'x (a-blank))
               (s-try _ (s-block _ (list (s-id _ 'a)))
                           (s-bind _ #f 'e (a-blank))
                           (s-block _ (list (s-num _ 5))))))
))

(define ids-and-vars (test-suite "ids-and-vars"
  (check/block "x" (s-id _ 'x))
  (check/block "_foo" (s-id _ '_foo))
  (check/block "x = 5"
    (s-let _ (s-bind _ #f 'x (a-blank)) (s-num _ 5)))

  (check/block "x := 1" (s-assign _ 'x (s-num _ 1)))

  (check/block "var g = 5"
               (s-var _ (s-bind _ #f 'g (a-blank)) (s-num _ 5)))

  (check/block "x = 5 y = 10"
    (s-let _ (s-bind _ #f 'x (a-blank)) (s-num _ 5))
    (s-let _ (s-bind _ #f 'y (a-blank)) (s-num _ 10)))

  (check/block "duplicates-ok-in-parse = 5 duplicates-ok-in-parse = 10"
    (s-let _ (s-bind _ #f 'duplicates-ok-in-parse (a-blank)) (s-num _ 5))
    (s-let _ (s-bind _ #f 'duplicates-ok-in-parse (a-blank)) (s-num _ 10)))

  (check/block "x = 5 x"
    (s-let _ (s-bind _ #f 'x (a-blank)) (s-num _ 5))
    (s-id _ 'x))

  (check-parse/fail "var x = x = 5" "parsing error")

  (check/block "x :: Number = 22"
    (s-let _ (s-bind _ #f 'x (a-name _ 'Number)) (s-num _ 22)))
))

(define check-blocks (test-suite "check-blocks"
   (check/block "check: 1 end"
                (s-check _ (s-block _ (list (s-num _ 1)))))
   (check/block "fun foo(): check: 1 end end"
                (s-fun _ 'foo (list) (list) (a-blank) ""
                       (s-block _
                        (list
                         (s-check _ (s-block _ (list (s-num _ 1))))))
                       (s-block _ (list))))

))


(define binary-operators (test-suite "binary-operators"
   (check/block "1 + 2" (s-op _ op+ (s-num _ 1) (s-num _ 2)))
   (check/block "1 + 2 + 3" (s-op _ op+ (s-op _ op+ (s-num _ 1) (s-num _ 2))
                                            (s-num _ 3)))
   ;; next two are invalid, to be caught in
   ;; well-formedness of ast check, pre-desugar
   (check/block "1 + 2 - 3" (s-op _ op- (s-op _ op+ (s-num _ 1) (s-num _ 2))
                                            (s-num _ 3)))
   (check/block "1 + 2 * 3" (s-op _ op* (s-op _ op+ (s-num _ 1) (s-num _ 2)) (s-num _ 3)))

   (check/block "1 + o.x" (s-op _ op+
                                (s-num _ 1)
                                (s-dot _ (s-id _ 'o) 'x)))

   (check/block "1 + 2.add(3)" (s-op _ op+
                                     (s-num _ 1)
                                     (s-app _ (s-dot _ (s-num _ 2) 'add)
                                                      (list (s-num _ 3)))))
   (check/block "2.add(3) + 1" (s-op _ op+
                                     (s-app _ (s-dot _ (s-num _ 2) 'add)
                                            (list (s-num _ 3)))
                                     (s-num _ 1)))
   (check/block "1 + 2.add(3) + 4" (s-op _ op+
                                         (s-op _ op+
                                               (s-num _ 1)
                                               (s-app _ (s-dot _ (s-num _ 2) 'add)
                                                      (list (s-num _ 3))))
                                         (s-num _ 4)))
   (check/block "(1 - 2) + 3" (s-op _ op+
                                    (s-paren _
                                        (s-op _ op-
                                          (s-num _ 1)
                                          (s-num _ 2)))
                                    (s-num _ 3)))
   (check/block "(3 * (1 - 2)) / 3"
                (s-op _ op/
                      (s-paren _ (s-op _ op*
                            (s-num _ 3)
                            (s-paren _
                                (s-op _ op-
                                  (s-num _ 1)
                                  (s-num _ 2)))))
                      (s-num _ 3)))
   (check/block "x = 3 + 4"
                (s-let _ (s-bind _ #f 'x _) (s-op _ op+ (s-num _ 3) (s-num _ 4))))
   (check/block "3 + if true: 7 end"
                (s-op _ op+
                      (s-num _ 3)
                      (s-if _ (list
                                 (s-if-branch _ (s-bool _ #t)
                                                (s-block _ (list (s-num _ 7))))))))

   (check/block "1 + (2 * 3)"
                (s-op _ op+
                      (s-num _ 1)
                      (s-paren _ (s-op _ op* (s-num _ 2) (s-num _ 3)))))

   (check/block "1 * (2 - 3)"
                (s-op _ op*
                      (s-num _ 1)
                      (s-paren _ (s-op _ op- (s-num _ 2) (s-num _ 3)))))

   (check/block "1 / (2 * 3)"
                (s-op _ op/
                      (s-num _ 1)
                      (s-paren _ (s-op _ op* (s-num _ 2) (s-num _ 3)))))

   (check/block "1 - (2 * 3)"
                (s-op _ op-
                      (s-num _ 1)
                      (s-paren _ (s-op _ op* (s-num _ 2) (s-num _ 3)))))

   (check/block "foo((2 + 3))"
                (s-app _
                       (s-id _ 'foo)
                       (list
                        (s-paren _ (s-op _ op* (s-num _ 2) (s-num _ 3))))))

   (check/block "fun f(y):
                  y
                end
                f((1 + 2))" _ _)

   (check/block "foo((2 + 3) * 2)"
                (s-app _
                       (s-id _ 'foo)
                       (list
                        (s-op _ op* (s-paren _ (s-op _ op+ (s-num _ 2) (s-num _ 3)))
                              (s-num _ 2)))))

   (check/block "1 < 2"
                (s-op _ op< (s-num _ 1) (s-num _ 2)))

   (check/block "1 > 2"
                (s-op _ op> (s-num _ 1) (s-num _ 2)))

   (check/block "1 <= 2"
                (s-op _ op<= (s-num _ 1) (s-num _ 2)))

   (check/block "1 >= 2"
                (s-op _ op>= (s-num _ 1) (s-num _ 2)))

   (check/block "1 == 2"
                (s-op _ op== (s-num _ 1) (s-num _ 2)))

   (check/block "1 <> 2"
                (s-op _ op<> (s-num _ 1) (s-num _ 2)))

   (check/block "1 <= (1 + 2)"
                (s-op _ op<= (s-num _ 1)
                      (s-paren _ (s-op _ op+ (s-num _ 1) (s-num _ 2)))))

   (check/block "(a + ((d * 2) + g))"
                (s-paren _ (s-op _ op+
                                   (s-id _ 'a)
                                   (s-paren _ (s-op _ op+ (s-paren _ (s-op _ op* (s-id _ 'd) (s-num _ 2)))
                                                          (s-id _ 'g))))))

   (check-parse/fail "when(1 < 2): 3" "parsing error")

   (check/block "(a == b) or (true)"
                (s-op _ opor (s-paren _ (s-op _ op==
                                              (s-id _ 'a)
                                              (s-id _ 'b)))
                             (s-paren _ (s-bool _ #t))))
   (check/block "((1))"
                (s-paren _ (s-paren _ (s-num _ 1))))
   (check/block "(((1)))"
                (s-paren _ (s-paren _ (s-paren _ (s-num _ 1)))))
   (check/block "((((1))))"
                (s-paren _ (s-paren _ (s-paren _ (s-paren _ (s-num _ 1))))))

   (check/block "true and false"
                (s-op _ opand (s-bool _ #t) (s-bool _ #f)))

   (check/block "(1 < 2) and false"
                (s-op _ opand (s-paren _ (s-op _ op< (s-num _ 1) (s-num _ 2))) (s-bool _ #f)))

   (check/block "false or (1 < 2)"
                (s-op _ opor (s-bool _ #f) (s-paren _ (s-op _ op< (s-num _ 1) (s-num _ 2)))))

   (check/block "not false" (s-not _ (s-bool _ #f)))

   (check/block "a.b and c.d"
                (s-op _ opand _ _))

   (check/block "a(b) or c(d)"
                (s-op _ opor _ _))

   (check/block "not (true and false)"
                (s-not _
                       (s-paren _
                        (s-op _ opand
                              (s-bool _ #t)
                              (s-bool _ #f)))))

   (check-parse/fail "true and not false" "parsing error")

   (check-parse/fail "true and false and not true and false" "parsing error")


   (check/block "not a.b" (s-not _ (s-dot s _ _)))

   (check/block "not a(b)" (s-not _ _))

   (check/block "o raises e" (s-check-test _ 'opraises (s-id _ 'o) (s-id _ 'e)))
   (check/block "o is e" (s-check-test _ 'opis (s-id _ 'o) (s-id _ 'e)))
   (check/block "o satisfies e" (s-check-test _ 'opsatisfies (s-id _ 'o) (s-id _ 'e)))
   ))

(define semis (test-suite "semis"
  (check/block "fun: expr ;"
               (s-lam _ empty (list)
                 (a-blank)
                 _
                 (s-block _ (list))
                 _))

  (check/block "for iter(thing from somewhere): dostuff();"
      (s-for _
           (s-id _ 'iter)
           (list (s-for-bind _ (s-bind _ #f 'thing (a-blank)) (s-id _ 'somewhere)))
           (a-blank)
           (s-block _ (list
                        (s-app _
                               (s-id _ 'dostuff)
                               (list))))))

  (check/block "when not(false): if true: blowup() else: false;;"
      (s-when _
        (s-not _ (s-bool _ #f))
        (s-block _
          (list
            (s-if-else _
              (list
                (s-if-branch _ (s-bool _ #t)
                               (s-block _ (list (s-app _ (s-id _ 'blowup) (list))))))
              (s-block _ (s-bool _ #f)))))))

   (check/block "data D: | var1();" (s-data _ 'D (list) (list) (list (s-variant _ 'var1 (list) (list))) _ _))

   (check/block "{ m(x): 5;, m2(self): 6; }"
                (s-obj _
                  (list
                    (s-method-field _ (s-str _ "m") (list (s-bind _ #f 'x (a-blank))) _ _ (s-block _ (list (s-num _ 5))) _)
                    (s-method-field _ (s-str _ "m2") (list (s-bind _ #f 'self (a-blank))) _ _ (s-block _ (list (s-num _ 6))) _))))


))

(define all (test-suite "all"
  tokenizer
  literals
  methods
  functions
  check-blocks
  fields
  annotations
  anon-func
  cases
  data
  graph
  user-block
  for
  modules
  caret
  exceptions
  ids-and-vars
  binary-operators
))

(run-tests all 'normal)
