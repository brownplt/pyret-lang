#lang racket

(require "test-utils.rkt" "../lang/ast.rkt")

(define-syntax check/block
  (syntax-rules ()
    [(_ str stmt ...)
     (check-match (parse-pyret str) (s-prog _ empty (s-block _ (list stmt ...))))]))

(check/block "'str'" (s-str _ "str"))
(check/block "5" (s-num _ 5))
(check/block "-5" (s-num _ -5))

(check/block "true" (s-bool _ #t))
(check/block "false" (s-bool _ #f))

(check/block "5 'foo'" (s-num _ 5) (s-str _ "foo"))

(check/block "{}" (s-obj _ empty))
(check/block "{x:5}" (s-obj _ (list (s-data-field _ (s-str _ "x") (s-num _ 5)))))
(check/block "{x:5,y:'foo'}" (s-obj _ (list (s-data-field _ (s-str _ "x") (s-num _ 5))
                                                            (s-data-field _ (s-str _ "y") (s-str _ "foo")))))
(check/block "{x:{}}" (s-obj _ (list (s-data-field _ (s-str _ "x") (s-obj _ empty)))))

(check/block "{[x]:5}" (s-obj _ (list (s-data-field _ (s-id _ 'x) (s-num _ 5)))))
(check/block "{['x']:5}" (s-obj _ (list (s-data-field _ (s-str _ "x") (s-num _ 5)))))

(check/block "x" (s-id _ 'x))
(check/block "{x:x}" (s-obj _ (list (s-data-field _ (s-str _ "x") (s-id _ 'x)))))

(check/block "{f(): x}"
             (s-obj _ (list (s-method-field _ (s-str _ "f") (list) (a-blank) (s-block _ (list (s-id _ 'x)))))))
(check/block "{f(): x end}"
             (s-obj _ (list (s-method-field _ (s-str _ "f") (list) (a-blank) (s-block _ (list (s-id _ 'x)))))))

(check/block "{[f](): x}"
             (s-obj _ (list (s-method-field _ (s-id _ 'f)
                                            (list) (a-blank) (s-block _ (list (s-id _ 'x)))))))
(check/block "{['f'](): x}"
             (s-obj _ (list (s-method-field _ (s-str _ "f")
                                            (list) (a-blank) (s-block _ (list (s-id _ 'x)))))))

(check/block "f()" (s-app _ (s-id _ 'f) empty))
(check/block "f(5)" (s-app _ (s-id _ 'f) (list (s-num _ 5))))

(check/block "f(5,'foo')" (s-app _ (s-id _ 'f) (list (s-num _ 5) (s-str _ "foo"))))

(check/block "fun f(): 5 end"
            (s-fun _ 'f empty empty (a-blank) _ (s-block _ (list (s-num _ 5)))))
(check/block "fun f(): (5)"
            (s-fun _ 'f empty empty (a-blank) _ (s-block _ (list (s-num _ 5)))))

(check/block "fun g(g): 5 end"
            (s-fun _ 'g empty (list (s-bind _ 'g (a-blank))) (a-blank)
                   _
                   (s-block _ (list (s-num _ 5)))))

(check/block "fun g(g,f,x): 5 end"
             (s-fun _ 'g empty (list (s-bind _ 'g (a-blank)) 
                                     (s-bind _ 'f (a-blank)) 
                                     (s-bind _ 'x (a-blank))) (a-blank)
                    _
                    (s-block _ (list (s-num _ 5)))))

(check/block "fun g(g,f,x): (5)"
             (s-fun _ 'g empty (list (s-bind _ 'g (a-blank)) 
                               (s-bind _ 'f (a-blank)) 
                               (s-bind _ 'x (a-blank))) (a-blank)
                    _
                    (s-block _ (list (s-num _ 5)))))

(check/block "var g: 5"
             (s-var _ (s-bind _ 'g (a-blank)) (s-num _ 5)))

(check/block "[]" (s-list _ empty))
(check/block "[5]" (s-list _ (list (s-num _ 5))))

(check/block "o.x" (s-dot _ (s-id _ 'o) 'x))
(check/block "seal({x:5}, []).x"
             (s-dot _ (s-app _ (s-id _ 'seal) (list (s-obj _ (list (s-data-field _ (s-str _ "x") (s-num _ 5))))
                                                    (s-list _ empty)))
                    'x))

(check/block "o.['x']" (s-bracket _ (s-id _ 'o) (s-str _ "x")))
(check/block "x := 1" (s-assign _ 'x (s-num _ 1)))
(check/block "o.x = 1" (s-dot-assign _ (s-id _ 'o) 'x (s-num _ 1)))
(check/block "o.['x'] = 1" 
            (s-bracket-assign _ (s-id _ 'o) (s-str _ "x") (s-num _ 1)))

(check/block "brander()"
             (s-app _ (s-id _ 'brander) (list)))

(check/block "3.add" (s-dot _ (s-num _ 3) 'add))

(check/block "{extend {x:5} with y:3}"
             (s-onion _
                      (s-obj _ (list (s-data-field _ (s-str _ "x") (s-num _ 5))))
                      (list (s-data-field _ (s-str _ "y") (s-num _ 3)))))

(check/block "{extend 5 with foo: 12}"
             (s-onion _
                      (s-num _ 5)
                      (list (s-data-field _ (s-str _ "foo") (s-num _ 12)))))

(check/block "{extend List with length: 0, width: 0}"
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

(check/block "var x :: Number: 5" (s-var _ (s-bind _ 'x (a-name _ 'Number))
                                           (s-num _ 5)))
(check/block "var x :: Number: 'hello'" (s-var _ (s-bind _ 'x (a-name _ 'Number))
                                         (s-str _ "hello")))

(check/block "var f :: (Number, Number -> Number): plus"
	     (s-var _ (s-bind _ 'f (a-arrow _
					    (list (a-name _ 'Number) (a-name _ 'Number))
					    (a-name _ 'Number)))
		    (s-id _ 'plus)))

(check/block "fun foo(x) -> (Number -> Number): 'should return a function from num to num' end" 
             (s-fun _ 'foo empty (list (s-bind _ 'x (a-blank))) (a-arrow _ (list (a-name _ 'Number)) (a-name _ 'Number))
                    _
                    (s-block _ (list (s-str _ _)))))
(check/block "fun foo(x :: Bool) -> Bool: x end" 
             (s-fun _ 'foo empty (list (s-bind _ 'x (a-name _ 'Bool))) (a-name _ 'Bool)
                    _
                    (s-block _ (list (s-id _ 'x)))))

(check/block "var x :: {}: 4" (s-var _ (s-bind _ 'x (a-record _ (list))) (s-num _ 4)))
(check/block "var x :: {foo: Number}: 4" 
             (s-var _ (s-bind _ 'x (a-record _ (list (a-field _ "foo" Number)))) (s-num _ 4)))
(check/block "var x :: {foo: Number}: 4" 
             (s-var _ (s-bind _ 'x (a-record _ (list (a-field _ "foo" Number)))) (s-num _ 4)))
(check/block "var x :: {foo: Number, a: Bool}: 4" 
             (s-var _ (s-bind _ 'x (a-record _ (list (a-field _ "foo" (a-name _ 'Number)) 
                                                     (a-field _ "a" (a-name _ 'Bool)))))
                    (s-num _ 4)))

(check/block " \\(x)"
             (s-lam _ empty (list)
                    (a-blank)
                    _
                    (s-block _ (list (s-id _ 'x)))))

(check/block " \\-> Number: (x)"
             (s-lam _ empty (list)
                    (a-name _ 'Number)
                    _
                    (s-block _ (list (s-id _ 'x)))))

(check/block "\\x :: Number, y :: Bool -> Number: (x.send(y))"
             (s-lam _ empty (list (s-bind _ 'x (a-name _ 'Number))
                            (s-bind _ 'y (a-name _ 'Bool)))
                    (a-name _ 'Number)
                    _
                    (s-block _ (list (s-app _
                                            (s-dot _ (s-id _ 'x) 'send)
                                            (list (s-id _ 'y)))))))

(check/block "\\x,y,z: (x)"
             (s-lam _ empty (list (s-bind _ 'x (a-blank))
                            (s-bind _ 'y (a-blank))
                            (s-bind _ 'z (a-blank)))
                    (a-blank)
                    _
                    (s-block _ (list (s-id _ 'x)))))

(check/block "cond: | true => 1 | false => 2 end" 
             (s-cond _ (list (s-cond-branch _ (s-bool _ #t) (s-block _ (list (s-num _ 1))))
                             (s-cond-branch _ (s-bool _ #f) (s-block _ (list (s-num _ 2)))))))

(check/block "  data Foo | bar end"
             (s-data _ 'Foo empty (list (s-variant _ 'bar (list) (list))) (list)))

(check/block "data NumList
  | empty
  | cons: first :: Number, rest :: NumList
end"
             (s-data _ 'NumList (list) (list (s-variant _ 'empty (list) (list))
                                             (s-variant _ 'cons (list (s-member _ 'first (a-name _ 'Number))
                                                                      (s-member _ 'rest (a-name _ 'NumList)))
                                                        (list)))
                     (list)))

(check/block "var my-hypthen-y-ident-i-fier: 10"
             (s-var _ (s-bind _ 'my-hypthen-y-ident-i-fier (a-blank)) (s-num _ 10)))

(check/block "data List(a) | empty end" (s-data _ 'List (list 'a) (list (s-variant _ 'empty (list) (list))) (list)))

(check/block 
 "data List(a) | cons: field, l :: List<a> end" 
 (s-data _ 'List (list 'a) 
         (list (s-variant 
                _ 
                'cons 
                (list (s-member _ 'field (a-blank)) 
                      (s-member _ 'l (a-app _ 'List (list (a-name _ 'a)))))
                (list))) (list)))


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

(check/block
 "fun (a) f(x :: a) -> a: x end"
 (s-fun _ 'f (list 'a) (list (s-bind _ 'x (a-name _ 'a))) (a-name _ 'a)
  _
	(s-block _ (list (s-id _ 'x)))))

(check/block
 "fun (a,b) f(x :: a) -> b: x end"
 (s-fun _ 'f (list 'a 'b) (list (s-bind _ 'x (a-name _ 'a))) (a-name _ 'b)
  _
	(s-block _ (list (s-id _ 'x)))))


(check/block
 "fun (a,b) f(x :: a) -> b: 'doc' x end"
 (s-fun _ 'f (list 'a 'b) (list (s-bind _ 'x (a-name _ 'a))) (a-name _ 'b)
  "doc"
	(s-block _ (list (s-id _ 'x)))))


(check/block
 "data Foo | bar with x(self): self end"
 (s-data _ 'Foo (list)
         (list (s-variant _ 'bar (list)
                          (list (s-method-field _
                                                (s-str _ "x")
                                                (list (s-bind _ 'self (a-blank)))
                                                (a-blank)
                                                (s-block _ (list (s-id _ 'self)))))))
         (list)))

(check/block
 "data Foo | bar with x(self) -> Num: self
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
                                                              (s-block _ (list (s-id _ 'self)))))))
         (list (s-data-field _ (s-str _ "z") (s-num _ 10)))))

(check/block
 "o.{x : 5}"
 (s-onion _ (s-id _ 'o) (list (s-data-field _ (s-str _ "x") (s-num _ 5)))))

(check/block
 "{x : 5}.{x : 10, y : 6}"
 (s-onion _ (s-obj _ (list (s-data-field _ (s-str _ "x") (s-num _ 5))))
          (list (s-data-field _ (s-str _ "x") (s-num _ 10))
                (s-data-field _ (s-str _ "y") (s-num _ 6)))))


(check-match (parse-pyret "import 'file.arr' as file")
 (s-prog _ (list (s-import _ "file.arr" 'file)) (s-block _ (list))))

(check-match (parse-pyret "provide {a: 1} end")
 (s-prog _ (list (s-provide _ (s-obj _ (list (s-data-field _ (s-str _ "a") (s-num _ 1))))))
 	   (s-block _ (list))))

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

;; non-empty lists for x
(check/block "var x :: List(list.is-cons): 4"
  (s-var _
         (s-bind _ 'x
                   (a-pred _
                           (a-name _ 'List)
                           (s-dot _ (s-id _ 'list)
                                    'is-cons)))
         (s-num _ 4)))

;; non-empty lists of strings for x
(check/block "var x :: List<String>(list.is-cons): 4"
  (s-var _
         (s-bind _ 'x
                   (a-pred _
                           (a-app _ 'List
                                    (list (a-name _ 'String)))
                           (s-dot _ (s-id _ 'list)
                                    'is-cons)))
         (s-num _ 4)))

