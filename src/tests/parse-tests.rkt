#lang racket

(require
  rackunit
  rackunit/text-ui
  srfi/13
  "test-utils.rkt"
  "../lang/ast.rkt"
  "../lang/pretty.rkt"
  "../lang/desugar.rkt")

(define verbose #f)
(set! verbose #f)

(define round-trip-test #f)
(set! round-trip-test #f)

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
       (when verbose
         (printf "Testing: \n~a\n\n" str))
       ;; NOTE(dbp): because we expect there to be whitespace before paren exprs,
       ;; in test context (where there is no #lang), we prepend everything with " "
       (check-match (parse-pyret (string-append " " str))
                    (s-prog _ empty (s-block _ (list stmt ...))))
       (when round-trip-test
           (check-not-exn
            (lambda ()
              (define pr (pretty (desugar-pyret (parse-pyret str))))
              (when verbose
                (printf "Printed to: \n~a\n\n" pr))
              (parse-pyret pr)))))]))

(define literals (test-suite "literals"
  (check/block "'str'" (s-str _ "str"))
  (check/block "5" (s-num _ 5))
  (check/block "-7" (s-num _ -7))
  (check/block "10.2" (s-num _ 10.2))
  (check/block "-10.2" (s-num _ -10.2))

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
               (s-method _ (list (s-bind _ 'x (a-blank)) (s-bind _ 'y (a-blank)))
                         (a-blank) _ (s-block _ (list (s-num _ 1))) _))

  (check/block "method(self): 1 check foo end"
               (s-method _ (list (s-bind _ 'self (a-blank)))
                         (a-blank) _ (s-block _ (list (s-num _ 1)))
                         (s-block _ (list (s-id _ 'foo)))))

  (check/block "method(self): 1 check end"
               (s-method _ (list (s-bind _ 'self (a-blank)))
                         (a-blank) _ (s-block _ (list (s-num _ 1)))
                         (s-block _ empty)))

  (check/block "{f(): x check 1 end}"
               (s-obj _ (list (s-method-field _ (s-str _ "f") (list) (a-blank) _
                                              (s-block _ (list (s-id _ 'x)))
                                              (s-block _ (list (s-num _ 1)))))))

  (check/block "method(self): doc 'hello' 1 end"
               (s-method _ (list (s-bind _ 'self (a-blank)))
                         (a-blank) "hello" (s-block _ (list (s-num _ 1)))
                         (s-block _ empty)))

  (check/block "{f(): doc 'hello' 1 end}"
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
              (s-fun _ 'g empty (list (s-bind _ 'g (a-blank))) (a-blank)
                     _
                     (s-block _ (list (s-num _ 5)))
                     (s-block _ empty)))

  (check/block "fun g(g,f,x): 5 end"
               (s-fun _ 'g empty (list (s-bind _ 'g (a-blank)) 
                                       (s-bind _ 'f (a-blank)) 
                                       (s-bind _ 'x (a-blank))) (a-blank)
                      _
                      (s-block _ (list (s-num _ 5)))
                      (s-block _ empty)))

  (check/block "brander()"
               (s-app _ (s-id _ 'brander) (list)))

  (check/block "fun f(): 5 check 4 end"
               (s-fun _ 'f empty empty (a-blank) _
                      (s-block _ (list (s-num _ 5)))
                      (s-block _ (list (s-num _ 4)))))

  (check/block "fun f(): 5 check end"
               (s-fun _ 'f empty empty (a-blank) _
                      (s-block _ (list (s-num _ 5)))
                      (s-block _ empty)))
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

  (check/block "{x:5}.{y:3}"
               (s-onion _
                        (s-obj _ (list (s-data-field _ (s-str _ "x") (s-num _ 5))))
                        (list (s-data-field _ (s-str _ "y") (s-num _ 3)))))

  (check/block "5.{foo: 12}"
               (s-onion _
                        (s-num _ 5)
                        (list (s-data-field _ (s-str _ "foo") (s-num _ 12)))))

  (check/block "List.{length: 0, width: 0}"
               (s-onion _
                        (s-id _ 'List)
                        (list (s-data-field _ (s-str _ "length") (s-num _ 0))
                              (s-data-field _ (s-str _ "width") (s-num _ 0)))))

  (check/block "o:f"
               (s-dot-method _
                             (s-id _ 'o)
                             'f))

  (check/block "{f:4}:f"
               (s-dot-method _
                             (s-obj _ (list (s-data-field _ (s-str _ "f") (s-num _ 4))))
                             'f))
  (check/block
   "o.{x : 5}"
   (s-onion _ (s-id _ 'o) (list (s-data-field _ (s-str _ "x") (s-num _ 5)))))

  (check/block
   "{x : 5}.{x : 10, y : 6}"
   (s-onion _ (s-obj _ (list (s-data-field _ (s-str _ "x") (s-num _ 5))))
            (list (s-data-field _ (s-str _ "x") (s-num _ 10))
                  (s-data-field _ (s-str _ "y") (s-num _ 6)))))

))


(define annotations (test-suite "annotations"
  (check/block
   "fun <a> f(x :: a) -> a: x end"
   (s-fun _ 'f (list 'a) (list (s-bind _ 'x (a-name _ 'a))) (a-name _ 'a)
    _
    (s-block _ (list (s-id _ 'x)))
    (s-block _ empty)))

  (check/block
   "fun <a,b> f(x :: a) -> b: x end"
   (s-fun _ 'f (list 'a 'b) (list (s-bind _ 'x (a-name _ 'a))) (a-name _ 'b)
    _
    (s-block _ (list (s-id _ 'x)))
    (s-block _ empty)))


  (check/block
   "fun <a,b> f(x :: a) -> b: doc 'some documentation' x end"
   (s-fun _ 'f (list 'a 'b) (list (s-bind _ 'x (a-name _ 'a))) (a-name _ 'b)
    "some documentation"
    (s-block _ (list (s-id _ 'x)))
    (s-block _ empty)))


  (check/block "fun foo(x) -> (Number -> Number): 'should return a function from num to num' end" 
               (s-fun _ 'foo empty (list (s-bind _ 'x (a-blank))) (a-arrow _ (list (a-name _ 'Number)) (a-name _ 'Number))
                      _
                      (s-block _ (list (s-str _ _)))
                      (s-block _ empty)))
  (check/block "fun foo(x :: Bool) -> Bool: x end" 
               (s-fun _ 'foo empty (list (s-bind _ 'x (a-name _ 'Bool))) (a-name _ 'Bool)
                      _
                      (s-block _ (list (s-id _ 'x)))
                      (s-block _ empty)))

  (check/block "var x :: Number = 5" (s-var _ (s-bind _ 'x (a-name _ 'Number))
                                             (s-num _ 5)))
  (check/block "var x :: Number = 'hello'" (s-var _ (s-bind _ 'x (a-name _ 'Number))
                                           (s-str _ "hello")))

  (check/block "var f :: (Number, Number -> Number) = plus"
         (s-var _ (s-bind _ 'f (a-arrow _
                (list (a-name _ 'Number) (a-name _ 'Number))
                (a-name _ 'Number)))
          (s-id _ 'plus)))


  (check/block "var x :: {} = 4" (s-var _ (s-bind _ 'x (a-record _ (list))) (s-num _ 4)))
  (check/block "var x :: {foo: Number} = 4" 
               (s-var _ (s-bind _ 'x (a-record _ (list (a-field _ "foo" Number)))) (s-num _ 4)))
  (check/block "var x :: {foo: Number} = 4" 
               (s-var _ (s-bind _ 'x (a-record _ (list (a-field _ "foo" Number)))) (s-num _ 4)))
  (check/block "var x :: {foo: Number, a: Bool} = 4" 
               (s-var _ (s-bind _ 'x (a-record _ (list (a-field _ "foo" (a-name _ 'Number)) 
                                                       (a-field _ "a" (a-name _ 'Bool)))))
                      (s-num _ 4)))
  (check/block "var x :: list.List = 4"
               (s-var _ (s-bind _ 'x (a-dot _ 'list 'List))
                      (s-num _ 4)))
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
               (s-lam _ empty (list (s-bind _ 'x (a-name _ 'Number))
                              (s-bind _ 'y (a-name _ 'Bool)))
                      (a-name _ 'Number)
                      _
                      (s-block _ (list (s-app _
                                              (s-dot _ (s-id _ 'x) 'send)
                                              (list (s-id _ 'y)))))
                      _))

  (check/block "fun(x,y,z): x end"
               (s-lam _ empty (list (s-bind _ 'x (a-blank))
                              (s-bind _ 'y (a-blank))
                              (s-bind _ 'z (a-blank)))
                      (a-blank)
                      _
                      (s-block _ (list (s-id _ 'x)))
                      _))

  (check/block "fun(x): x check foo end"
               (s-lam _ empty (list (s-bind _ 'x (a-blank)))
                      (a-blank)
                      _
                      (s-block _ (list (s-id _ 'x)))
                      (s-block _ (list (s-id _ 'foo)))))

  

  ;; non-empty lists for x
  (check/block "var x :: List(list.is-cons) = 4"
    (s-var _
           (s-bind _ 'x
                     (a-pred _
                             (a-name _ 'List)
                             (s-dot _ (s-id _ 'list)
                                      'is-cons)))
           (s-num _ 4)))

  ;; non-empty lists of strings for x
  (check/block "var x :: List<String>(list.is-cons) = 4"
    (s-var _
           (s-bind _ 'x
                     (a-pred _
                             (a-app _ 'List
                                      (list (a-name _ 'String)))
                             (s-dot _ (s-id _ 'list)
                                      'is-cons)))
           (s-num _ 4)))
))

(define cases (test-suite "cases"
  (check/block "case: | true => 1 | false => 2 end" 
               (s-case _ (list (s-case-branch _ (s-bool _ #t) (s-block _ (list (s-num _ 1))))
                               (s-case-branch _ (s-bool _ #f) (s-block _ (list (s-num _ 2)))))))

  (check/block "when true: 5 end"
    (s-when _ (s-bool _ #t) (s-block _ (list (s-num _ 5)))))

  (check/block "when x.equals(42): print('got it') print('the answer') end"
    (s-when _ (s-app _ (s-dot _ (s-id _ 'x) 'equals) (list (s-num _ 42)))
      (s-block _ (list
        (s-app _ (s-id _ 'print) (list (s-str _ "got it")))
        (s-app _ (s-id _ 'print) (list (s-str _ "the answer")))))))

))

(define data (test-suite "data"

  (check/block "  data Foo: | bar() end"
               (s-data _ 'Foo empty (list (s-variant _ 'bar (list) (list))) (list)))

  (check/block "data NumList:
    | empty()
    | cons(first :: Number, rest :: NumList)
  end"
               (s-data _ 'NumList (list) (list (s-variant _ 'empty (list) (list))
                                               (s-variant _ 'cons (list (s-bind _ 'first (a-name _ 'Number))
                                                                        (s-bind _ 'rest (a-name _ 'NumList)))
                                                          (list)))
                       (list)))
  (check/block "data List<a>: | empty() end" (s-data _ 'List (list 'a) (list (s-variant _ 'empty (list) (list))) (list)))

  (check/block 
   "data List<a>: | cons(field, l :: List<a>) end" 
   (s-data _ 'List (list 'a) 
           (list (s-variant 
                  _ 
                  'cons 
                  (list (s-bind _ 'field (a-blank)) 
                        (s-bind _ 'l (a-app _ 'List (list (a-name _ 'a)))))
                  (list))) (list)))

  (check/block
   "data List: | empty end"
   (s-data _ 'List (list)
    (list (s-singleton-variant
           _
           'empty
           (list)))
    (list)))

  (check/block
   "data List: | empty with length(self): 0 end end"
   (s-data _ 'List (list)
    (list (s-singleton-variant
           _
           'empty
           (list (s-method-field _ (s-str _ "length") (list (s-bind _ 'self (a-blank))) (a-blank) _ (s-block _ (list (s-num _ 0))) _))))
    (list)))

  (check/block
   "data Foo: | bar() with x(self): self end end"
   (s-data _ 'Foo (list)
           (list (s-variant _ 'bar (list)
                            (list (s-method-field _
                                                  (s-str _ "x")
                                                  (list (s-bind _ 'self (a-blank)))
                                                  (a-blank)
                                                  _
                                                  (s-block _ (list (s-id _ 'self)))
                                                  _))))
           (list)))

  (check/block
   "data Foo: | bar() with x(self) -> Num: self end
    sharing
      z: 10
    end"
   (s-data _ 'Foo (list) (list (s-variant _
                                          'bar
                                          (list)
                                          (list (s-method-field _
                                                                (s-str _ "x")
                                                                (list (s-bind _ 'self (a-blank)))
                                                                (a-name _ 'Num)
                                                                _
                                                                (s-block _ (list (s-id _ 'self)))
                                                                _))))
           (list (s-data-field _ (s-str _ "z") (s-num _ 10)))))

))

(define doblock (test-suite "doblock"
  (check/block
   "do while x.lessthan(10); x.add(1) end"
   (s-do _
         (s-id _ 'while)
         (list (s-block _ (list
                           (s-app _
                                  (s-dot _ (s-id _ 'x) 'lessthan)
                                  (list (s-num _ 10)))))
               (s-block _ (list (s-app _
                                       (s-dot _ (s-id _ 'x) 'add)
                                       (list (s-num _ 1))))))))
))

(define for (test-suite "for"

  (check/block
    "for map(elt from lst): elt.plus(42) end"
    (s-for _
           (s-id _ 'map)
           (list (s-for-bind _ (s-bind _ elt (a-blank)) (s-id _ 'lst)))
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
            (s-for-bind _ (s-bind _ acc (a-name _ 'Number))
                          (s-num _ 0))
            (s-for-bind _ (s-bind _ elt (a-name _ 'Number))
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
      (s-bind _ 'e (a-blank))
      (s-block _ (list (s-num _ 5)))))

  (check/block "try: try: a except(e): 5 end b c except(e): 5 end"
    (s-try _
      (s-block _ (list
        (s-try _ (s-block _ (list (s-id _ 'a)))
                 (s-bind _ 'e (a-blank))
                 (s-block _ (list (s-num _ 5))))
        (s-id _ 'b)
        (s-id _ 'c)))
      (s-bind _ 'e (a-blank))
      (s-block _ (list (s-num _ 5)))))
))

(define ids-and-vars (test-suite "ids-and-vars"
  (check/block "x" (s-id _ 'x))
  (check/block "_foo" (s-id _ '_foo))
  (check/block "x = 5"
    (s-let _ (s-bind _ 'x (a-blank)) (s-num _ 5)))

  (check/block "x := 1" (s-assign _ 'x (s-num _ 1)))

  (check/block "var g = 5"
               (s-var _ (s-bind _ 'g (a-blank)) (s-num _ 5)))

  (check/block "x = 5 y = 10"
    (s-let _ (s-bind _ 'x (a-blank)) (s-num _ 5))
    (s-let _ (s-bind _ 'y (a-blank)) (s-num _ 10)))

  (check/block "duplicates-ok-in-parse = 5 duplicates-ok-in-parse = 10"
    (s-let _ (s-bind _ 'duplicates-ok-in-parse (a-blank)) (s-num _ 5))
    (s-let _ (s-bind _ 'duplicates-ok-in-parse (a-blank)) (s-num _ 10)))

  (check/block "x = 5 x"
    (s-let _ (s-bind _ 'x (a-blank)) (s-num _ 5))
    (s-id _ 'x))

  (check-parse/fail "var x = x = 5" "parsing error")

  (check/block "x :: Number = 22"
    (s-let _ (s-bind _ 'x (a-name _ 'Number)) (s-num _ 22)))
))

(define binary-operators (test-suite "binary-operators"
   (check/block "1 + 2" (s-op _ op+ (s-num _ 1) (s-num _ 2)))
   (check/block "1 + 2 + 3" (s-op _ op+ (s-op _ op+ (s-num _ 1) (s-num _ 2))
                                            (s-num _ 3)))
   ;; next two are invalid, to be caught in
   ;; well-formedness of ast check, pre-desugar
   (check/block "1 + 2 - 3" (s-op _ op- (s-op _ op+ (s-num _ 1) (s-num _ 2))
                                            (s-num _ 3)))
   (check/block "1 + 2 * 3" (s-op _ op* (s-op _ op+ (s-num _ 1) (s-num _ 2))
                                            (s-num _ 3)))
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
                (s-let _ (s-bind _ 'x _) (s-op _ op+ (s-num _ 3) (s-num _ 4))))
   (check/block "3 + case: |true => 7 end"
                (s-op _ op+
                      (s-num _ 3)
                      (s-case _ (list
                                 (s-case-branch _ (s-bool _ #t)
                                                (s-block _ (list (s-num _ 7))))))))

   (check/block "1+(2*3)"
                (s-op _ op+
                      (s-num _ 1)
                      (s-paren _ (s-op _ op* (s-num _ 2) (s-num _ 3)))))
   
   (check/block "1*(2-3)"
                (s-op _ op*
                      (s-num _ 1)
                      (s-paren _ (s-op _ op- (s-num _ 2) (s-num _ 3)))))

   (check/block "1/(2*3)"
                (s-op _ op/
                      (s-num _ 1)
                      (s-paren _ (s-op _ op* (s-num _ 2) (s-num _ 3)))))

   (check/block "1-(2*3)"
                (s-op _ op-
                      (s-num _ 1)
                      (s-paren _ (s-op _ op* (s-num _ 2) (s-num _ 3)))))

   (check/block "foo((2+3))"
                (s-app _
                       (s-id _ 'foo)
                       (list
                        (s-paren _ (s-op _ op* (s-num _ 2) (s-num _ 3))))))

   (check/block "fun f(y):
                  y
                end
                f((1+2))" _ _)

   (check/block "foo((2+3)*2)"
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

   (check/block "1 <= (1+2)"
                (s-op _ op<= (s-num _ 1)
                      (s-paren _ (s-op _ op+ (s-num _ 1) (s-num _ 2)))))
   
   (check/block "(a + ((d * 2) + g))"
                (s-paren _ (s-op _ op+
                                   (s-id _ 'a)
                                   (s-paren _ (s-op _ op+ (s-paren _ (s-op _ op* (s-id _ 'd) (s-num _ 2)))
                                                          (s-id _ 'g))))))

   (check-parse/fail "when(1 < 2): 3" "parsing error")

   (check/block "(a == b).or(true)"
                (s-app _ (s-dot _ (s-paren _ (s-op _ op==
                                                       (s-id _ 'a)
                                                       (s-id _ 'b)))
                                    'or)
                       (list (s-bool _ #t))))
   (check/block "((1))"
                (s-paren _ (s-paren _ (s-num _ 1))))
   (check/block "(((1)))"
                (s-paren _ (s-paren _ (s-paren _ (s-num _ 1)))))
   (check/block "((((1))))"
                (s-paren _ (s-paren _ (s-paren _ (s-paren _ (s-num _ 1))))))
   ))

(define all (test-suite "all"
  literals
  methods
  functions
  fields
  annotations
  anon-func
  cases
  data
  for
  doblock
  modules
  caret
  exceptions
  ids-and-vars
  binary-operators
))

(run-tests all 'normal)

