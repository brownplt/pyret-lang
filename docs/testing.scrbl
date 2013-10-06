#lang scribble/manual

@(require "common.rkt")

@title[#:tag "s:testing"]{Testing}

Pyret's definition forms---@prod{data-expr} and @prod{fun-expr}---both
support @in-code{where:} blocks intended to hold tests for the definition.  In
addition, top-level @in-code{check:} blocks can hold tests.  These blocks are
handled specially when the program is run in @emph{check mode}, which is the
default mode for running Pyret programs.

@section[#:tag "s:check/where"]{Check Mode Desugaring}

When running in check mode, Pyret scans the statements of each @prod{block} in
the program for @in-code{check:} blocks and definitions with attached
@in-code{where:} blocks.  It collects these blocks and sets them up to be run
at the @emph{end} of the block it finds them in, so they are run after all
functions and data definitions have already been defined.

This happens to all inner blocks as we,, so @in-code{check:} and
@in-code{where:} blocks in nested scopes (say for helper functions) are run
@emph{each} time the function is called.

So, for example, in this program, 14 tests are run.  The comments show how that
number accumulates:

@justcode{
fun exp(x :: Number, y :: Number):
  when y < 0:
    raise("Cannot take negative exponents")
  end
  if y == 0:
    1
  else:
    fun times-x(n :: Number0):
      x * n
    where:
      times-x(4) is (4 * x)
      timex-x(10) is (10 * x)
    end
    times-x(exp(x, y - 1))
  end
where:
  exp(3, 2) is 9  # inside exp, the else: block is evaluated twice,
                  # so there are 2 invocations of times-x's tests,
                  # and 4 successes
                  # Including this test, after this line, 5 tests
                  # have passed (0 + 5)

  exp(4, 3) is 64 # inside exp, the else: block is evaluated three times,
                  # so there are 3 invocations of times-x,
                  # and 6 successes.
                  # Including this test, after this line, 12 tests
                  # have passed (5 + 7)

  exp(5, 0) is 1  # inside exp, the else: block is not evaluated, 
                  # so the times-x where: block is not run
                  # Including this test, after this line, 13 tests
                  # have been run (12 + 1)
                  
  exp(3, -1) raises "Cannot take negative exponents"
                  # inside exp, the else: block is not evaluated,
                  # so the times-x where: block is not run
                  # Including this test, after this line, 14 tests
                  # have been run (13 + 1)
end
}

@section[#:tag "s:checkers"]{Check Mode Helpers}

There are two syntactic forms that are helpful for testing programs inside
@tt{check:} and @tt{where:} blocks.

@(subsection-title @bold{@tt{is}} "s:is")

The @tt{is} form accepts two expressions on either side of the @tt{is} operator:

@justcode{
expr "is" expr
}

This first evaluates the left expression, and then the right.  If either
signals an exception, the whole @tt{is} test fails with that exception as the
reason.  If both expressions evaluate to values, they are compared for
equality.  If the equality check succeeds, the test is registered as a success,
otherwise it fails with a message about the two values not being equal.

Under the hood, the @tt{is} form is calling the built-in library function
@tt{checkers.check-is} [REF].


@(subsection-title @bold{@tt{raises}} "s:raises")

The @tt{raises} form accepts two expressions on either side of the @tt{raises}
operator:

@justcode{
expr "raises" expr
}

This first evaluates the expression on the @emph{right} and expects it to be a
string.  It then evaluates the expression on the left.  If the left expression
completes without error, the test fails with a message indicating that an
exception was expected and none occurred.  If the left expression signals an
exception, the exceptions value is compared against the provided string as
follows:

@itemlist[
  @item{
    If the exception value is a string, @tt{raises} checks for containment of
    the provided test string in the exception value.
  }

  @item{
    If the exception value is an error object, @tt{raises} checks for
    containment of the provided test string in the @tt{message} field of the
    exception value.
  }
]



