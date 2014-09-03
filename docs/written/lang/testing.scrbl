#lang scribble/base

@(require scribble/core
          scribble/decode
          (only-in scribble/manual link)
          scriblib/footnote
          "../abbrevs.rkt"
          "../../scribble-api.rkt"
          scribble/html-properties)

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


@(append-gen-docs
  '(module "testing"
    (path "src/js/base/runtime-anf.js")))

@docmodule["testing" #:friendly-title "Testing"]{

@section[#:tag "testing-blocks"]{@pyret{check:} and @pyret{where:} blocks}

Tests in Pyret are written in special @emph{testing blocks}.  These blocks can
contain any Pyret code that isn't toplevel-only (like data definitions and
import or provide statements), and are the only places where
@seclink["testing-operators" "Testing Operators"] can be used.

@subsection{@pyret{check:} blocks}

The simplest testing blocks are @pyret{check:} blocks.  They can be written at
the top-level or inside other testing blocks.  Check blocks are a unit of
reporting test results, so all the test operators that evaluate inside a check
block will be reported as part of that block.  For example, these two check
blocks:

@pyret-block{
check "a first block":
  5 is 5
  4 is 5
end

check "a second block":
  6 is 7
end
}

will report:

@verbatim{
Check block: a first block
  test (5 is 5): ok
  test (4 is 5): failed, reason:
    Values not equal:
    4
    5
  1/2 tests passed in check block: a first block

Check block: a second block
  test (6 is 7): failed, reason:
    Values not equal:
    6
    7
  The test failed.

1/3 tests passed in all check blocks
}


Testing blocks are also a unit of failure: most of the time an error stops the
whole program, but inside a check block (and also inside @pyret-id{raises},
mentioned later), the error is stopped and reported, and Pyret goes on to
evaluating the next check block:

@pyret-block{
check "error-block":
  raise("an error here doesn't stop the next check block from running")
  string-length("this test doesn't run") is 21
end

check "a later block":
  string-length("these tests still run") is 21
end
}

Keep an eye out for the message @pyret{"Check block <some-block> ended in an
error (all tests may not have run):"}, because it means that later tests in the
same block may not have run, and they may have failed.

@subsection{@pyret{where:} blocks}

Sometimes a function has tests that are explicitly associated with it.  For
these cases, the function can end in a @pyret{where:} block rather than
immediately with @pyret{end}.  @pyret{where:} blocks run the same way that
@pyret{check:} blocks do, and their name is taken from the function they are
attached to.

@examples{
fun double(n):
  n + n
where:
  double(10) is 20
  double(15) is 30
end
}

@section[#:tag "testing-operators"]{Testing Operators}

Testing operators must be written on their own line inside a 


@subsection{Binary Test Operators}

Many useful tests compare two values, whether for a specific type of
@seclink["equality" "equality"] or a more sophisticated predicate.

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

@test-doc-pred["is-not%" "pred" "expr1" "expr2"]

Like @pyret-id{is%}, but failure and success are reversed.

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

Evaluates @pyret{expr} to a value and @pyret{pred} to a value expected to be a
function (if not a function, an error is thrown).  Then, @pyret{pred(val)} is
evaluated, and if the result is @pyret{true}, the test succeeds, and if
@pyret{false}, the test fails.

@test-doc["violates" "expr" "pred"]

Like @pyret-id{satisfies}, but failure and success are reversed.

@examples{
check:
  [list:] satisfies is-empty
  [list:] satisfies lam(l): l.length() == 0 end

  is-odd = lam(n :: Number): num-modulo(n, 2) == 1 end
  5 satisfies is-odd
  6 violates is-odd
end
}


@subsection{Exception Test Operators}

@test-doc["raises" "expr" "exn-string"]

Evaluates @pyret{expr} and expects an error to be raised.  If no error is
raised, the test fails.

If an error is the result, the @pyret-id["torepr" "<global>"] function is
called on the exception value, and @pyret-id{raises} checks that
@pyret{exn-string} is contained within that string.  If so, the test passes,
otherwise, it fails.

For simple errors (like those in many programming assignments), it works to use
@pyret-id["raise" "<global>"] on a string value and check that that string is
raised.  For larger programs, it can be useful to construct more sophisticated
error values and use @pyret-id{raises-satisfies} to test them.

@examples{
check:
  raise("the roof!") raises "the roof"

  string-length("too", "many", "strings") raises "arity-mismatch"

  {}.x raises "field-not-found"
end
}

@bold{Warning!} These two tests are not equivalent:

@pyret-block{
check "actually catches the error":
  raise("error!") raises "error!"
end

check "error happens before raises":
  value = raise("error!")
  value raises "error!"
end
}

This is because the left-hand-side of @pyret-id{raises} is a special position
that can detect and catch errors, which normal expressions do not do.  So the
second check block fails before even getting to the @pyret-id{raises} line; try
it out and see what happens.

@test-doc["raises-other-than" "expr" "exn-string"]

Like @pyret-id{raises}, but the result must @emph{not} contain @pyret{exn-string}.

@test-doc1["does-not-raise" "expr"]

Evaluates @pyret{expr} and checks that no error is raised while evaluating
it.  The expression can evaluate to any value.

@test-doc["raises-satisfies" "expr" "pred"]

As the name suggests, this combines the idea of @pyret-id{raises} with
@pyret-id{satisfies} and calls @pyret{pred} on the exception that
@pyret{expr} raises (if any).  Still fails if no exception is raised.

@examples{
import is-field-not-found from error
check:
  o = {}
  o.x raises-satisfies E.is-field-not-found
end
}

@test-doc["raises-violates" "expr" "pred"]

Like @pyret-id{raises-satisfies}, but the predicate must return
@pyret{false}.  Still fails if no exception is raised.

}
