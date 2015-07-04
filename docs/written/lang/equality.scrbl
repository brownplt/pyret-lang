#lang scribble/base

@(require scribble/core
          scribble/decode
          (only-in scribble/manual link)
          scriblib/footnote
          "../abbrevs.rkt"
          "../../scribble-api.rkt"
          scribble/html-properties)

@(define boolean '(a-id "Boolean" (xref "<global>" "Boolean")))
@(define eq '(a-id "EqualityResult" (xref "equality" "EqualityResult")))
@(define eqfun `(a-arrow ,A ,A ,B))
@(define numeqfun `(a-arrow ,N ,N ,B))
@(define eq3fun `(a-arrow ,A ,A ,eq))
@(define T (a-id "EqualityResult" (xref "equality" "EqualityResult")))

@(append-gen-docs
  `(module "equality"
    (path "src/js/base/runtime-anf.js")
    (fun-spec
      (name "equal-now")
      (arity 2)
      (args ("val1" "val2"))
      (return ,boolean)
      (doc ""))
    (fun-spec
      (name "equal-always")
      (arity 2)
      (args ("val1" "val2"))
      (return ,boolean)
      (doc ""))
    (fun-spec
      (name "identical")
      (arity 2)
      (args ("val1" "val2"))
      (return ,boolean)
      (doc ""))
    (fun-spec
      (name "within-abs")
      (arity 1)
      (args ("tol"))
      (return ,eqfun)
      (doc ""))
    (fun-spec
      (name "within-abs-now")
      (arity 1)
      (args ("tol"))
      (return ,eqfun)
      (doc ""))
    (fun-spec
      (name "within")
      (arity 1)
      (args ("tol"))
      (return ,eqfun)
      (doc ""))
    (fun-spec
      (name "within-rel")
      (arity 1)
      (args ("tol"))
      (return ,eqfun)
      (doc ""))
    (fun-spec
      (name "within-rel-now")
      (arity 1)
      (args ("tol"))
      (return ,eqfun)
      (doc ""))
    (fun-spec
      (name "within3")
      (arity 1)
      (args ("tol"))
      (return ,eq3fun)
      (doc ""))
    (fun-spec
      (name "within-abs3")
      (arity 1)
      (args ("tol"))
      (return ,eq3fun)
      (doc ""))
    (fun-spec
      (name "within-abs-now3")
      (arity 1)
      (args ("tol"))
      (return ,eq3fun)
      (doc ""))
    (fun-spec
      (name "within-rel3")
      (arity 1)
      (args ("tol"))
      (return ,eq3fun)
      (doc ""))
    (fun-spec
      (name "within-rel-now3")
      (arity 1)
      (args ("tol"))
      (return ,eq3fun)
      (doc ""))
    (fun-spec
      (name "equal-now3")
      (arity 2)
      (args ("val1" "val2"))
      (return ,eq)
      (doc ""))
    (fun-spec
      (name "equal-always3")
      (arity 2)
      (args ("val1" "val2"))
      (return ,eq)
      (doc ""))
    (fun-spec
      (name "identical3")
      (arity 2)
      (args ("val1" "val2"))
      (return ,eq)
      (doc ""))
  ))

@(define code pyret)

@(define equal-now-op @code{=~})
@(define equal-always-op @code{==})
@(define identical-op @code{<=>})

@docmodule["equality"]{

@section{Types of Equality}

Pyret has three notions of equality.  Two values can be @emph{equal now},
@emph{always equal}, and/or @emph{identical}.  The following table summarizes
the functions and operators that test for these relationships, and how they
compare to some other languages' operators:

@tabular[
  #:style (style #f (list (attributes '((style . "border-collapse: collapse;")))))
  #:column-properties (list (list (attributes '((style . "border: 1px solid black; padding: 5px;")))))
  (list
    (list
      @list{@bold{Name}}
      @list{@bold{Operator}}
      @list{@bold{Partial Predicate}}
      @list{@bold{Total Predicate}}
      @list{@bold{Similar To}}
    )
    (list
      @list{@emph{Equal Now}}
      @list{@code{=~}}
      @list{@code{equal-now}}
      @list{@code{equal-now3}}
      @list{@code{equal?} (Racket) @code{==} (Python, Ruby)}
    )
    (list
      @list{@emph{Always Equal}}
      @list{@code{==}}
      @list{@code{equal-always}}
      @list{@code{equal-always3}}
      @list{@code{=} (Ocaml)}
    )
    (list
      @list{@emph{Identical}}
      @list{@code{<=>}}
      @list{@code{identical}}
      @list{@code{identical3}}
      @list{
        @code{eq?} (Scheme)
        @code{==} (Ocaml)
        @code{===} (JavaScript)
        @code{is} (Python)
        @code{==} (Java)
      }
    )
    )
]

In most programs, you should use @emph{always equal}, or @code{==}, to compare
values that you want to check for same-ness.  If you are working with mutable
data, you may want to consider the special behavior of @emph{equal now}.  For
some optimizations, defensive code, and capability patterns, you may have a
reason to use @emph{identical}.

@section{Equal Now}

@function["equal-now" #:contract (a-arrow A A B)]

Checks if the two values are equal @emph{now} (they may not be later).
Corresponds to the @equal-now-op operator.

@subsection[#:tag "s:equal-now-primitives"]{Equal Now and Primitives}

@code{equal-now} checks primitive equality on numbers, strings, and
booleans:

@examples{
check:
  5 is%(equal-now) 5
  5 is-not%(equal-now) 6
  "abc" is%(equal-now) "abc"
  "a" is-not%(equal-now) "b"
  "a" is-not%(equal-now) 5
end
}

@subsection[#:tag "s:equal-now-structural"]{Equal Now and Structured Data}

For instances of @code{data} (including, for example, instances of
@pyret-id["List" "lists"]), and objects, @pyret-id{equal-now} traverses their
members and checks for pairwise equality.  So, for example, lists will
recursively check that their contents are the same, including the case where
their contents are objects:

@examples{
check:
  l1 = [list: 1, 2, 3]
  l2 = [list: 1, 2, 3]

  l1 is%(equal-now) l2
  link(1, l1) is-not%(equal-now) l2

  l3 = [list: {x: 5}]
  l4 = [list: {x: 5}]
  l5 = [list: {x: 6}]
  l3 is%(equal-now) l4
  l3 is-not%(equal-now) l5
end
}

@subsection[#:tag "s:equal-now-mutable"]{Equal Now and References}

Equal Now checks the contents of mutable data it reaches.  This gives it its
name: since it only checks the @emph{current} values, and those fields might
change, it is not true that if @code{e1 =~ e2}, then later @code{e1 =~ e2} will
hold again.  For example:

@examples{
data MyBox:
  | my-box(ref x)
end

check:
  b1 = my-box(1)
  b2 = my-box(1)

  b1 is%(equal-now) b2
  b1!{x : 2}

  b1 is-not%(equal-now) b2
end
}

Equal Now will recognize when references form a cycle, and cycles of the same
shape are recognized as equal (even though the references might change their
contents later):

@examples{
data InfiniteList:
  | i-link(first, ref rest)
  | i-empty
end

check:
  l1 = i-link(1, i-empty)
  l2 = i-link(1, i-empty)
  l3 = i-link(1, i-link(2, i-empty))
  l1!{rest : l1}
  l2!{rest : l2}
  l3!rest!{rest : l3}

  l1 is%(equal-now) l2
  l1 is-not%(equal-now) l3
end
}

@section{Identical}

@function["identical" #:contract (a-arrow A A B)]

@subsection[#:tag "s:identical-primitives"]{Identical and Primitives}

Identical has the same behavior on primitives as Equal Now
(@secref["s:equal-now-primitives"]).

@subsection[#:tag "s:identical-structural"]{Identical and Structural Equality}

Identical does not visit members of objects or data instances.  Instead, it
checks if the values are actually the same exact value (the operator is meant
to indicate that the values are interchangable).  So objects with the same
fields are not identical to anything but themselves:

@examples{
check:
  o = { x: 5 }
  o2 = { x: 5 }
  o is-not%(identical) o2
  o is%(identical) o
  o2 is%(identical) o2
end
}

@subsection[#:tag "s:identical-mutable"]{Identical and Mutable Data}

Identical does not inspect the contents of mutable data, either.  It can be
used to tell if two references are @emph{aliases} for the same underlying
state, or if they are in fact different (even though they may be equal right
now).

@examples{
data InfiniteList:
  | i-link(first, ref rest)
  | i-empty
end

check:
  l1 = i-link(1, i-empty)
  l2 = i-link(1, i-empty)
  l1!{rest : l1}
  l2!{rest : l2}

  l1 is%(identical) l1
  l1!rest is%(identical) l1
  l1 is-not%(identical) l2
  l1!rest is-not%(identical) l2

  l2 is%(identical) l2
  l2!rest is%(identical) l2
  l2 is-not%(identical) l1
  l2!rest is-not%(identical) l1
end
}

@;{
  Identical differs from the other equality operators on mutable data in that
  on frozen immutable data, it does not inspect the contents of the reference.
  Instead, it checks that the two references are in fact the same reference.
  So, for example, the behavior of the @code{graph:} example from above
  differs:

@pyret-block{
  data MList:
    | mlink(ref first, ref rest)
    | mempty
  end
  mlist = {
    make: fun(arr):
      # fold mlink over arr
    end
  }

  graph:
    BOS = [mlist: WOR, PROV]
    PROV = [mlist: BOS]
    WOR = [mlist: BOS]
  end

  graph:
    SF = [mlist: OAK, MV]
    MV = [mlist: SF]
    OAK = [mlist: SF]
  end

  SF isnot%(identical) BOS
  PROV isnot%(identical) WOR
  PROV isnot%(identical) OAK
  OAK isnot%(identical) PROV

  BOS!rest is%(identical) PROV
  BOS is%(identical) BOS
}
}

@section{Always Equal}

@function["equal-always" #:contract (a-arrow A A B)]

Checks if the two values will always be equal, and corresponds to the
@equal-always-op operator.

@code{equal-always} checks for primitive and structural equality like
@pyret-id{equal-now}, with the exception that it stops at mutable data and only
checks that the mutable values are @pyret-id{identical}.  Stopping at mutable
boundaries ensures that if two values were @pyret-id{equal-always} at any
point, they will still be @pyret-id{equal-always} later.

@subsection[#:tag "s:always-equal-mutable"]{Always Equal and Mutable Data}

Here are some examples of @pyret-id{equal-always} stopping at mutable data, but
checking immutable data, contrasted with @pyret-id{equal-now}

@pyret-block{
data MyBox:
  | my-box(ref x)
end

check:
  b1 = my-box(1)
  b2 = my-box(1)

  b1 is-not%(equal-always) b2
  b1 is%(equal-now) b2
  b2!{x : 2}

  b1 is-not%(equal-always) b2
  b1 is-not%(equal-now) b2

  b3 = my-box(2)

  # remember that b2 currently contains 2
  l1 = [list: b1, b2]
  l2 = [list: b1, b2]
  l3 = [list: b1, b3]

  l1 is%(equal-now) l2
  l1 is%(equal-always) l2
  l1 is-not%(identical) l2

  l1 is%(equal-now) l3
  l1 is-not%(equal-always) l3
  l1 is-not%(identical) l3

  b2!{x: 5}

  l1 is%(equal-now) l2
  l1 is%(equal-always) l2
  l1 is-not%(identical) l2

  l1 is-not%(equal-now) l3
  l1 is-not%(equal-always) l3
  l1 is-not%(identical) l3
end
}

@;{
@subsection[#:tag "s:always-equal-frozen"]{Always Equal and Frozen Mutable Data}

  Mutable references can be @emph{frozen}[REF] (as with @code{graph:}), which
  renders them immutable.  @code{equal-always} @emph{will} traverse frozen
  mutable fields, and will check for same-shaped cycles.  So, for example, it
  will succeed for cyclic graphs created with @code{graph:} that have the same
  shape:

@pyret-block{
  data MList:
    | mlink(ref first, ref rest)
    | mempty
  end
  mlist = {
    make: fun(arr):
      # fold mlink over arr
    end
  }

  graph:
    BOS = [mlist: WOR, PROV]
    PROV = [mlist: BOS]
    WOR = [mlist: BOS]
  end

  graph:
    SF = [mlist: OAK, MV]
    MV = [mlist: SF]
    OAK = [mlist: SF]
  end

  SF is%(equal-now) BOS
  PROV is%(equal-now) WOR
  PROV is%(equal-now) OAK
  OAK is%(equal-now) PROV
}
}

@section{Properties of Equality Functions}

The discussion above hints at a relationship between the three functions.  In
particular, if two values are Identical, they ought to be Always Equal, and if
they are Always Equal, they ought to be Equal Now.  The following table
summarizes this relationship, which in fact does hold:

@tabular[
  #:style (style #f (list (attributes '((style . "border-collapse: collapse;")))))
  #:column-properties (list (list (attributes '((style . "border: 1px solid black; padding: 5px;")))))
  (list
    (list
      @list{If ↓, then →}
      @list{@code{v1 <=> v2} could be...}
      @list{@code{v1 == v2} could be...}
      @list{@code{v1 =~ v2} could be...}
    )
    (list
      @list{@code{v1 <=> v2 is true}}
      "-"
      @;@list{@code{true} only}
      @list{@code{true} only}
      @list{@code{true} only}
    )
    (list
      @list{@code{v1 == v2 is true}}
      @list{@code{true} or @code{false}}
      "-"
      @;@list{@code{true} only}
      @list{@code{true} only}
    )
    (list
      @list{@code{v1 =~ v2 is true}}
      @list{@code{true} or @code{false}}
      @list{@code{true} or @code{false}}
      "-"
      @;@list{@code{true} only}
    )
    )
]

This table doesn't have all the @pyret{false} cases in it, because we need to
complete the story for a few values that haven't been discussed before we can
give the whole picture.

@section[#:tag "s:bounded-equalities"]{Bounded Equalities}

When comparing numbers, it's often useful to be able to compare within a
range.  For example, if we write an algorithm that computes an answer to
within a given tolerance, we may want to check if the answer is within that
tolerance.

@pyret-block{
check:
  sqrt-5 = num-sqrt(5)
  (sqrt-5 < 2.23) is true
  (sqrt-5 > 2.22) is true
end
}

Pyret has a family of built-in functions for cases like this, and the default
is @pyret{within}:

@function["within" #:contract (a-arrow N A)]

It takes an argument representing the @emph{relative error}, and returns a
function that can be used to check equality up to that relative error.  For
example, we can check if an answer (9.5) is within 10% of a
desired result (10.5):

@pyret-block{
check:
  within-10-percent = within(0.1)
  within-10-percent(9.5, 10.5)
end
}

The relative difference is defined as the absolute difference between the given
value and the desired value, divided by the absolute desired
value. The predicate returned by @pyret-id{within} checks that the
relative difference is no more than the supplied relative error.
Thus, in the expression above, @pyret-id{within} performs the
following check:

@pyret-block{
((10.5 - 9.5) / 10.5) < 0.1
}

@note{Converting to exact numbers first avoids overflows on computing the
mean.}
Put yet another way, aside from some slight differences in bounds checking for
errors, we could implement the numeric comparison of @pyret-id{within} as:

@pyret-block{
fun within(tol):
  lam(answer, desired):
    (num-abs(num-exact(answer) - num-exact(desired)) /
     num-abs(num-exact(desired))) <
    tol
  end
end
}

It's common to use @pyret{within} along with @pyret-id["is%" "testing"] to
define the binary predicate inline with the test:

@examples{
check:
  num-sqrt(10) is%(within(0.1)) 3.2
  num-sqrt(10) is-not%(within(0.1)) 5
end
}

Finally, @pyret-id{within} accepts @emph{any} two values, not just numbers.
On non-numeric arguments, @pyret-id{within} traverses the structures just as
in @pyret-id{equal-always}, but deferring to the bounds-checking equality when
a pair of numbers is encountered.  All other values are compared with
@pyret-id{equal-always}.

@examples{
check:
  l7 = [list: 1]
  l8 = [list: ~1.2]
  l7 is%(within-rel(0.5))  l8
  l7 is-not%(within-rel(0.1)) l8
  l7 is%(within-rel(~0.5))  l8
  l7 is-not%(within-rel(~0.1)) l8
end
}

@function["within-rel" #:contract (a-arrow N A)]

An alias for @pyret-id{within}.

@function["within-abs" #:contract (a-arrow N A)]

Like @pyret-id{within-rel}, but compares with @emph{absolute} tolerance rather
than relative.  The definition is equivalent to:

@pyret-block{
fun within-abs(tol):
  lam(left, right):
    num-abs(num-exact(left) - num-exact(right)) <= tol
  end
end
}

@examples{
check:
  la = [list: 10]
  lb = [list: ~12]
  la is%(within-abs(2))  lb
  la is-not%(within-abs(1))  lb
  la is%(within-abs(~5.5))  lb
  la is-not%(within-abs(~1.9999)) lb
end
}

@function["within-rel-now" #:contract (a-arrow N A)]
@function["within-abs-now" #:contract (a-arrow N A)]

Like @pyret-id{within-rel} and @pyret-id{within-abs}, but they traverse
mutable structures as in @pyret{equal-now}.

@examples{
check:
  aa = [array: 10]
  ab = [array: ~12]
  aa is%(within-rel-now(~0.2))  ab
  aa is-not%(within-rel(~0.2)) ab
  aa is%(within-abs-now(2))  ab
  aa is-not%(within-abs(2))  ab
end
}

@section[#:tag "s:undefined-equalities"]{Undefined Equalities}

For some values, Pyret refuses to report @pyret{true} or @pyret{false} for any
equality predicate, and raises an error instead.  For example:

@pyret-block{
check:
  (~3 == ~3) raises "equality-failure"

  (1 == ~1) raises "equality-failure"

  (lam(x): x end == lam(y): y end) raises "equality-failure"
end
}

This section discusses why this is the case.

@subsection[#:tag "s:roughnum-equality"]{Roughnums and Equality}

@note{@link["http://htdp.org/2003-09-26/Book/curriculum-Z-H-41.html#node_chap_33"]{How
to Design Programs} describes this design space well.} Numbers'
representations in programs reflect a number of tradeoffs, but the upshot is
that numbers have finite, approximate representations for performance reasons.
Numbers like @pyret{e}, @pyret{π}, and @pyret{√2} are only represented up to
some approximation of their true (irrational) value.  When such a result is
used in a computation, it represents a @emph{rough approximation} of the true
value.

Pyret calls these numbers @pyret-id["Roughnum" "numbers"]s, and they have
special rules related to equality.  In particular, they @emph{cannot} be
directly compared for equality, even if it seems like they ought to be equal:

@pyret-block{
check:
  (~3 == ~3) raises "equality-failure"
end
}

In addition, @pyret-id["Roughnum" "numbers"]s cannot be compared for equality
with @pyret-id["Exactnum" "numbers"]s, either.

@pyret-block{
check:
  (~0.1 == 0.1) raises "equality-failure"
end
}

@note{This example is not Pyret-specific, but matches the behavior of IEEE
floating point.}
Returning either @pyret{true} or @pyret{false} in this case would be
misleading.  This is due to the fact that because of unavoidable inaccuracies,
both of these expressions evaluate to @pyret{~0.1}, even though
mathematically only one of them does:

@pyret-block{
(~1 - ~0.9) + 0.00000000000000003
~0.2 - ~0.1
}

So in this check block, if we chose either @pyret{true} or @pyret{false} for
the result of @pyret{~0.1 == 0.1}, one test would be incorrect:

@pyret-block{
check:
  ((~1 - ~0.9) + 0.00000000000000003) is 0.1
  (~0.2 - ~0.1) is 0.1
end
}

If Pyret answered @pyret{true} for @pyret{~0.1 == ~0.1} (the rough
equivalent), then this test would pass when, mathematically, it ought to fail:

@pyret-block{
check:
  ((~1 - ~0.9) + 0.00000000000000003) is (~0.2 - ~0.1)
end
}

To avoid giving misleading answers in cases like these, Pyret triggers an
error on any number-to-number comparison that involves a @pyret-id["Roughnum"
"numbers"].  So, if you see an error like

@verbatim{
These two values cannot be compared for direct equality:

~0.1
~0.1

Approximations of numbers (Roughnums) cannot be compared for equality. The
program may need to use within().
}

It's a hint that the program should be using an equality from the
@pyret-id["within"] family of functions to do a relative comparison, rather
than a direct equality comparison.  So in this case, we could check that the
answer is equal up to an absolute error of @pyret{0.001}:

@pyret-block{
check:
  ((~1 - ~0.9) + 0.00000000000000003) is%(within(0.001)) (~0.2 - ~0.1)
end
}

If a program needs to check that two @pyret-id["Roughnum" "numbers"]s are
actually indistinguishable in their representation (even though they may be
approximating different values), it can do so by checking that the numbers are
within a tolerance of @pyret{~0}:

@pyret-block{
check:
  ((~1 - ~0.9) + 0.00000000000000003) is%(within(~0)) (~0.2 - ~0.1)
end
}

Note that the same won't work for a tolerance of @pyret{0}, the exact zero,
which will give an error if used to compare two @pyret-id["Roughnum"
"numbers"]s.

@subsection[#:tag "s:function-equality"]{Functions and Equality}

When comparing two functions or two methods, all the equality operators raise
an exception.  Why?  Well, the traditional way to compare functions for
equality (short of solving the halting problem), is to use reference equality
(or @pyret-id{identical}) on the functions' representations, the same way as
mutable data works.  For a hint of why this can be a misleading definition of
equality, consider this data definition:

@pyret-block{
data Stream<a>:
  | stream(first :: a, rest :: (-> Stream<a>))
end
check:
  fun mk-ones(): stream(1, mk-ones) end
  ones = mk-ones()
  ones is ones # Should this succeed?
  ones is mk-ones() # What about this?
  ones.rest() is mk-ones() # Or this...?
end
}

All of these values (@code{ones}, @code{mk-ones()}, etc.) have the same
behavior, so we could argue that @code{is} (which uses @code{==} behind the
scenes) ought to succeed on these.  And indeed, if we used reference equality,
it would succeed.  But consider this small tweak to the program:

@pyret-block{
check:
  fun mk-ones():
    stream(1, lam(): mk-ones() end)  # <-- changed this line
  end
  ones = mk-ones()
  ones is ones # Should this succeed?
  ones is mk-ones() # What about this?
  ones.rest() is mk-ones() # Or this...?
end
}

If we used reference equality on these functions, all of these tests would
now fail, and @code{ones} @emph{has the exact same behavior}.  Here's the
situation:

@note{In fact, a @link["http://en.wikipedia.org/wiki/Rice's_theorem"]{famous
result in theoretical computer science} is that it is impossible to figure out
out if two functions do the same thing in general, even if it is possible in
certain special cases (like reference equality).}

When reference equality returns @code{true}, we know that the two functions
must have the same behavior.  But when it returns @code{false}, we know
nothing.  The functions may behave exactly the same, or they might be
completely different, and the equality predicate can't tell us either way.

Pyret takes the following stance: You probably should rethink your program if
it relies on comparing functions for equality, since Pyret cannot give reliable
answers (no language can).  So, all the examples above (with one notable
exception) actually raise errors:

@pyret-block{
  check:
    fun mk-ones():
      stream(1, lam(): mk-ones() end)  # <-- changed this line
    end
    ones = mk-ones()
    ones == ones is true
    ones == mk-ones() raises "Attempted to compare functions"
    ones.rest() == mk-ones() raises "Attempted to compare functions"
  end
}

The first test is true because two @pyret-id{identical} values are considered
@pyret-id{equal-always}.  This is an interesting point in this design space
that Pyret may explore more in the future -- it isn't clear if the benefits of
this relationship between @pyret-id{identical} and @pyret-id{equal-always} are
worth the slight brittleness in the above example.

@para{
@bold{Note 1}: Functions can be compared with non-function values and return
@code{false}.  That is, the equality operators only throw the error if actual
function values need to be compared to one another, not if a function value is
compared to another type of value:
}

@pyret-block{
  check:
    f = lam(): "no-op" end
    g = lam(): "also no-op" end

    f == f raises "Attempted to compare functions"
    f == g raises "Attempted to compare functions"
    g == f raises "Attempted to compare functions"

    5 is-not%(equal-always) f

    { x: 5 } is-not%(equal-always) { x: f }
  end
}

@para{
  @bold{Note 2}: This rule about functions interacts with structural equality.
  When comparing two values, it seems at first unclear whether the result
  should be @code{false} or an error for this test:
  }

@pyret-block{
  check:
    { x: 5, f: lam(): "no-op" end } is%(equal-always)
      { x: 6, f: lam(): "no-op" end }
  end
}

This comparison will return @code{false}.  The rule is that if the equality
algorithm can find values that differ without comparing functions, it will
report the difference and return @code{false}.  However, if all of the
non-function comparisons are @code{true}, and some functions were compared,
then an error is raised.  A few more examples:

@pyret-block{

  check:
    o = { x: 5, y: { z: 6 }, lam(): "no-op" end }
    o2 = { x: 5, y: { z: 7 }, lam(): "no-op" end }

    (o == o) raises "Attempted to compare functions"
    o is-not%(equal-always) o2  # Test succeeds, because z fields differ
  end

}

@section[#:tag "s:total-equality-predicates"]{Total Equality Functions (Avoiding Incomparability Errors)}

Most Pyret programs should be written using @code{equal-always},
@code{equal-now}, and @code{identical}, which guarantee that an error will be
raised if functions are compared.  Some programs, however, need to be able to
compare arbitrary values, and it's convenient to have the ability to compare
values without raising an exception.  Since the equality of functions is
unknown, we define the result of a total equality check with a new datatype:

  @data-spec2["EqualityResult" (list) (list
  @singleton-spec2["EqualityResult" "Equal"]
  @constructor-spec["EqualityResult" "NotEqual" (list `("reason" ("type" "normal") ("contract" ,S)))]
  @singleton-spec2["EqualityResult" "Unknown"])]

  @nested[#:style 'inset]{
  @singleton-doc["EqualityResult" "Equal" T]
  @constructor-doc["EqualityResult" "NotEqual" (list `("reason" ("type" "normal") ("contract" ,S))) T]
  @singleton-doc["EqualityResult" "Unknown" T]

  @function["is-Equal" #:alt-docstrings ""]
  @function["is-NotEqual" #:alt-docstrings ""]
  @function["is-Unknown" #:alt-docstrings ""]
  }

We define three parallel functions to the equality predicates that return
@pyret-id{EqualityResult} values.  They return @pyret-id{Equal} and
@pyret-id{NotEqual} whenever the corresponding function would, and
@pyret-id{Unknown} whenever the corresponding function would throw an error:

  @function["equal-always3" #:contract (a-arrow A A T)]
  @function["equal-now3" #:contract (a-arrow A A T)]
  @function["identical3" #:contract (a-arrow A A T)]

@examples{
check:
  f = lam(): 5 end
  equal-always3(f, f) is Unknown
  equal-always3(f, 5) satisfies is-NotEqual
  equal-now3(f, f) is Unknown
  equal-now3("a", f) satisfies is-NotEqual
  identical3("a", f) satisfies is-NotEqual
  identical3(f, f) is Unknown
  identical3("a", f) satisfies is-NotEqual
end
}

We can now modify our table from above to be more complete:

@tabular[
  #:style (style #f (list (attributes '((style . "border-collapse: collapse;")))))
  #:column-properties (list (list (attributes '((style . "border: 1px solid black; padding: 5px;")))))
  (list
    (list
      @list{If ↓, then →}
      @list{@code{identical(v1, v2)} could be...}
      @list{@code{equal-always(v1, v2)} could be...}
      @list{@code{equal-now(v1, v2)} could be...}
    )
    (list
      @list{@code{identical(v1, v2) is Equal}}
      "-"
      @;@list{@code{Equal} only}
      @list{@code{Equal} only}
      @list{@code{Equal} only}
    )
    (list
      @list{@code{equal-always(v1, v2) is Equal}}
      @list{@code{Equal} or @code{NotEqual}}
      "-"
      @;@list{@code{Equal} only}
      @list{@code{Equal} only}
    )
    (list
      @list{@code{equal-now(v1, v2) is Equal}}
      @list{@code{Equal} or @code{NotEqual}}
      @list{@code{Equal} or @code{NotEqual}}
      "-"
      @;@list{@code{Equal} only}
    )
    (list "" "" "" "")
    (list
      @list{@code{identical(v1, v2) is NotEqual}}
      "-"
      @;@list{@code{NotEqual} only}
      @list{@code{Equal} or @code{NotEqual} or @code{Unknown}}
      @list{@code{Equal} or @code{NotEqual} or @code{Unknown}}
    )
    (list
      @list{@code{equal-always(v1, v2) is NotEqual}}
      @list{@code{NotEqual} only}
      "-"
      @;@list{@code{NotEqual} only}
      @list{@code{Equal} or @code{NotEqual} or @code{Unknown}}
    )
    (list
      @list{@code{equal-now(v1, v2) is NotEqual}}
      @list{@code{NotEqual} only}
      @list{@code{NotEqual} only}
      "-"
      @;@list{@code{NotEqual} only}
    )
    (list "" "" "" "")
    (list
      @list{@code{identical(v1, v2) is Unknown}}
      "-"
      @;@list{@code{Unknown} only}
      @list{@code{Unknown} only}
      @list{@code{Unknown} only}
    )
    (list
      @list{@code{equal-always(v1, v2) is Unknown}}
      @list{@code{Unknown} or @code{NotEqual}}
      "-"
      @;@list{@code{Unknown} only}
      @list{@code{Unknown} only}
    )
    (list
      @list{@code{equal-now(v1, v2) is Unknown}}
      @list{@code{Unknown} or @code{NotEqual}}
      @list{@code{Unknown} or @code{NotEqual}}
      "-"
      @;@list{@code{Unknown} only}
    )
    )
]

There are corresponding total functions defined for @pyret-id{within} as well:

@function["within3" #:contract (a-arrow N A)]
@function["within-rel3" #:contract (a-arrow N A)]
@function["within-rel-now3" #:contract (a-arrow N A)]
@function["within-abs3" #:contract (a-arrow N A)]
@function["within-abs-now3" #:contract (a-arrow N A)]

@section[#:tag "s:datatype-defined-equality"]{Datatype-defined Equality}

The functions @pyret-id{equal-now} and @pyret-id{equal-always} are defined to
work over values created with @pyret{data} by comparing fields in the same
position.  However, sometimes user-defined values need a more sophisticated
notion of equality than this simple definition provides.

For consider implementing an unordered @emph{set} of values in Pyret.  We might
choose to implement it as a function that creates an object closing over the
implementation of the set itself:

@pyret-block{
fun<a> make-empty-set():
  {
    add(self, element :: a): ... end,
    member(self, element :: a) -> Boolean: ... end,
    equal-to-other-set(self, other) -> Boolean: ... end
  }
end
}

We could fill in the bodies of the methods to have this implementation let
clients create sets and add elements to them, but it won't work well with
testing:

@pyret-block{
check:
  s = make-empty-set().add(5)
  s2 = make-empty-set().add(5)

  s.member(5) is true
  s2.member(5) is true

  s.equal-to-other-set(s2) is true

  s == s2 raises "Attempted to compare functions"
end
}

The final test raises an exception because it traverses the structure of the
object, and the only visible values are the three methods, which cannot be
compared.  We might just say that users of custom datatypes have to use custom
predicates for testing, for example they could write:

@pyret-block{
check:
  # as before ...
  fun equal-sets(set1, set2): set1.equal-to-other-set(set2) end
  s is%(equal-sets) s2
end
}

This works for sets on their own, but the built-in testing and equality
operators will not work with nested user-defined data structures.  For example,
since lists are a dataype that checks built-in equality on their members, a
list of sets as defined above will not use the equal-to-other-set method when
comparing elements, and give an @pyret{"Attempted to compare functions"} error:

@pyret-block{
check:
  # as before ...
  ([list: s] == [list: s2]) raises "Attempted to compare functions"
end
}

To help make this use case more pleasant, Pyret picks a method name to call, if
it is present, on user-defined objects when checking equality.  The method name
is @pyret{_equals}, and it has the following signature:

@(render-fun-helper '(method-spec)
  "_equals"
  (list 'part (tag-name (curr-module-name) "_equal"))
  (a-arrow "a" "a" (a-arrow A A EQ) EQ)
  EQ
  (list (list "self" "") (list "other" "") (list "equal-rec" ""))
  '()
  '()
  '())

Where @pyret{a} is the type of the object itself (so for sets, @pyret{other}
would be annotated with @pyret{Set<a>}).

The @pyret{_equals} method is called in the equality algorithm when:

@itemlist[
  @item{The two values are either both data values or both objects, AND}
  @item{If they are data values, the two values are of the same data type and
        variant, AND}
  @item{If they are objects not created by data, they have the same set of
  @seclink["brands"]}
]

So, for example, an object with an @pyret{_equals} method that always returns
@pyret-id{Equal} is not considered equal to values that aren't also objects:

@pyret-block{
import Equal from equality
check:
  eq-all = { _equals(self, other, eq): Equal end }
  eq-all is-not== f
  eq-all is-not== m
  eq-all is-not== 0
  eq-all is-not== "a"
  eq-all is== {}
end
}

The last argument to @pyret{_equals} is the recursive equality callback to use
for checking equality of any members.  When checking for equality of members
(say in our set implementation above), we would use this callback rather than
one of @pyret-id{equal-always3} or @pyret-id{equal-now3}.  The reasons for this
are threefold:

@itemlist[
  @item{In order to check for equality of cyclic values, Pyret needs to do
  internal bookkeeping of visited references.  This information is stored
  within the callback, and calling e.g. @pyret-id{equal-now3} directly would not
  take previously visted references into account.}

  @item{To avoid requiring datatypes to implement two equality methods, the
  callback also knows whether this equality call was started by
  @pyret-id{equal-now} or by @pyret-id{equal-always}.  Any recursive calls
  should use the original semantics for comparing references, so using the
  callback ensures that equality checks on elements have the right semantics
  (even in deeply nested data structures).}

  @item{The recursive equality predicate closes over and remembers the
  @emph{tolerance} for @pyret-id["within"]-family functions, and whether or
  not the tolerance is absolute or relative.}
]

}
