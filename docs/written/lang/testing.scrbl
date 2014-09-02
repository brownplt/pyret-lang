#lang scribble/base

@(require scribble/core
          scribble/decode
          (only-in scribble/manual link)
          scriblib/footnote
          "../abbrevs.rkt"
          "../../scribble-api.rkt"
          scribble/html-properties)

@title{Testing}

@(let ()
  (curr-module-name "<testing>"))

@section{@pyret{check:} and @pyret{where:} blocks}



@section{Testing Operators}

@(define (test-doc opname left right)
  @para[#:style "boxed pyret-header"]{
    @(tt @left " " @(make-header-elt-for (seclink (xref (curr-module-name) opname) (tt opname)) opname) " " @right)
  })

@(define (test-doc1 opname left)
  @para[#:style "boxed pyret-header"]{
    @(tt @left " " @(make-header-elt-for (seclink (xref (curr-module-name) opname) (tt opname)) opname))
  })


@(define (test-doc-pred opname pred left right)
  @para[#:style "boxed pyret-header"]{
    @(tt @left " " @(make-header-elt-for (seclink (xref (curr-module-name) opname) (tt opname)) opname) "(" pred ")" " " @right)
  })

@(define (test-pred-use left opname pred right)
  (list @pyret[left] " " @pyret-id[opname]@pyret{(}@|pred|@pyret{)} " " @pyret[left]))


@subsection{Binary Test Operators}

Many useful tests compare two values, whether for a specific type of
@seclink["Equality" "equality"] or a more sophisticated predicate.

@test-doc["is" "expr1" "expr2"]

Evaluates @pyret{expr1} and @pyret{expr2} to values, and checks if two values
are equal via @pyret-id["equal-always" "equality"], reporting success if they
are equal, and failure if they are not.

@test-doc["is-not" "expr1" "expr2"]

Like @pyret-id{is}, but failure and success are reversed.

@test-doc-pred["is%" "pred" "expr1" "expr2"]

Evaluates @pyret{expr1} and @pyret{expr2} to values, and @pyret{pred} to a
value that must be a function (an error is reported if @pyret{pred} is not a
function).  It then applies @pyret{pred} to the two values from @pyret{expr1}
and @pyret{expr2}.  If the result of that call is @pyret{true}, reports
success, otherwise reports failure.

This can be useful for comparing more subtle properties than equality, of which
there are many!

@examples{
check:
  fun less-than(n1, n2): n1 < n2 end

  1 is%(less-than) 2
  2 is-not%(less-than) 1
end

check:
  fun longer-than(s1, s2):
    string-length(s1) > string-length(s2)
  end

  "abc" is%(longer-than) "ab"
  "" is-not%(longer-than) ""
end

check:
  fun<a> equal-any-order(l1 :: List<a>, l2 :: List<a>):
    same-length = (l1.length() == l2.length())
    all-present = for lists.all(elt from l1):
      lists.member(l2, elt)
    end
    same-length and all-present
  end

  [list: 1, 2, 3] is%(equal-any-order) [list: 3, 2, 1]
  [list: 1, 2, 3] is%(equal-any-order) [list: 2, 1, 3]
  [list: 1, 2, 3, 3] is-not%(equal-any-order) [list: 2, 1, 3]
end

check:
  fun one-of(ans, elts):
    lists.member(elts, ans)
  end

  some-strings = [list: "123", "132", "213", "231", "312", "321"]
  "321" is%(one-of) some-strings
  "123" is%(one-of) some-strings
end

check:
  fun within(delta):
    lam(actual, target):
      num-abs(target - actual) <= delta
    end
  end

  5.05 is%(within(0.1)) 5
  5.00002 is-not%(within(0.00001)) 5
end
}

@test-doc-pred["is-not%" "pred" "expr1" "expr2"]

Like @pyret-id{is%}, but failure and success are reversed.

@test-doc["is==" "expr1" "expr2"]

Shorthand for @(test-pred-use "expr1" "is%" @pyret-id["equal-always" "equality"] "expr2")

@test-doc["is-not==" "expr1" "expr2"]

Like @pyret-id{is==}, but failure and success are reversed.

@test-doc["is=~" "expr1" "expr2"]

Shorthand for @(test-pred-use "expr1" "is%" @pyret-id["equal-now" "equality"] "expr2")

@test-doc["is-not=~" "expr1" "expr2"]

Like @pyret-id{is=~}, but failure and success are reversed.

@test-doc["is<=>" "expr1" "expr2"]

Shorthand for @(test-pred-use "expr1" "is%" @pyret-id["identical" "equality"] "expr2")

@test-doc["is-not<=>" "expr1" "expr2"]

Like @pyret-id{is<=>}, but failure and success are reversed.


@subsection{Unary Test Operators}

@test-doc["satisfies" "expr" "pred"]

@test-doc["violates" "expr" "pred"]

@subsection{Exception Test Operators}

@test-doc["raises" "expr" "exn-string"]

@test-doc["raises-other-than" "expr" "exn-string"]

@test-doc1["does-not-raise" "expr"]

@test-doc["raises-satisfies" "expr" "pred"]

@test-doc["raises-violates" "expr" "pred"]
