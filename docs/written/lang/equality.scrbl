#lang scribble/base

@(require scribble/core
          scribble/decode
          "../abbrevs.rkt"
          "../../scribble-api.rkt"
          scribble/html-properties)


@(append-gen-docs
  '(module "<equality>"
    (path "src/js/base/runtime-anf.js")
    (data-spec
      (name "Truth")
      (variants ("true" "false" "unknown")))
    (singleton-spec (name "true") (with-members ()))
    (singleton-spec (name "false") (with-members ()))
    (singleton-spec (name "unknown") (with-members ()))
    (fun-spec
      (name "equal-now")
      (arity 2)
      (args ("val1" "val2"))
      (doc ""))
    (fun-spec
      (name "equal-always")
      (arity 2)
      (args ("val1" "val2"))
      (doc ""))
    (fun-spec
      (name "identical")
      (arity 2)
      (args ("val1" "val2"))
      (doc ""))
    (fun-spec
      (name "equal-now-3")
      (arity 2)
      (args ("val1" "val2"))
      (doc ""))
    (fun-spec
      (name "equal-always-3")
      (arity 2)
      (args ("val1" "val2"))
      (doc ""))
    (fun-spec
      (name "identical-3")
      (arity 2)
      (args ("val1" "val2"))
      (doc ""))
))

@(let ()
  (curr-module-name "<equality>"))


@(define code tt)

@(define equal-now-op @code{"=~"})
@(define equal-always-op @code{"=="})
@(define identical-op @code{"<=>"})

@title{Equality}

@section{Types of Equality}

Pyret has three notions of equality.  Two values can be @emph{equal now},
@emph{always equal}, and/or @emph{identical}.  The following table summarizes
the functions and operators that test for these relationships, and how they
compare to some other languages' operators:

@tabular[
  #:style (style #f (list (attributes '((style . "border-collapse: collapse;")))))
  #:column-properties (list (list (attributes '((style . "border: 1px solid black;")))))
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
      @list{@code{equal-now-3}}
      @list{@code{equal?} (Racket) @code{==} (Python, Ruby)}
    )
    (list
      @list{@emph{Always Equal}}
      @list{@code{==}}
      @list{@code{equal-always}}
      @list{@code{equal-always-3}}
      @list{@code{=} (Ocaml)}
    )
    (list
      @list{@emph{Identical}}
      @list{@code{<=>}}
      @list{@code{identical}}
      @list{@code{identical-3}}
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
some optimizations, truly defensive code, and capability patterns, you may
have a reason to use @emph{identical}.

@section{Always Equal}
  @function["equal-always" #:contract (a-arrow A A B)]

  Checks if the two values will always be equal, and corresponds to the
  @equal-always-op operator.

  @code{equal-always} checks for structural equality of immutable data, raises
  exceptions on functions, and checks for identical-ness of mutable data.

@subsection[#:tag "s:always-equal-primitives"]{Always Equal and Primitives}

  @code{equal-always} checks primitive equality on numbers, strings, and
  booleans:

@pyret-block{
  check:
    5 is%(equal-always) 5
    5 isnot%(equal-always) 6
    "abc" is%(equal-always) "abc"
    "a" isnot%(equal-always) "b"
    "a" isnot%(equal-always) 5
  end
}

@subsection[#:tag "s:always-equal-structural"]{Always Equal and Structural Equality}

  For instances of @code{data} (including, for example, instances of
  @code{List}), and objects, @code{equal-always} traverses their members and
  checks for pairwise equality.  So, for example, lists will recursively check
  that their contents are the same, including the case where their contents
  are objects:

@pyret-block{
  check:
    l1 = [list: 1, 2, 3]
    l2 = [list: 1, 2, 3]

    l1 is%(equal-always) l2
    link(1, l1) isnot%(equal-always) l2

    l3 = [{x: 5}]
    l4 = [{x: 5}]
    l5 = [{x: 6}]
    l3 is%(equal-always) l4
    l3 isnot%(equal-always) l5
  end
}

  There are a few subtleties of structural equality that interact with mutable
  data and with 

@subsection[#:tag "s:always-equal-mutable"]{Always Equal and Mutable Data}

  So, for example, two instances of a datatype with a mutable field will test
  as not always equal:

@pyret-block{

  data MyBox:
    | my-box(ref x)
  end

  check:
    b1 = my-box(1)
    b2 = my-box(1)

    b1 is%(equal-always) b2 is false
    b1!{x : 2}

    b1 is%(equal-always) b2 is false
  end

}

  However, two instances of an immutable datatype will traverse their members
  when using @code{==}, so a list of boxes will compare its (potentially
  mutable) contents recursively with @code{equal-always}

@pyret-block{
  check:
    b1 = my-box(1)
    b2 = my-box(1)

    l1 = [list: b1, b2]
    l2 = [list: b1, b2]
    l3 = [list: b1, b1]

    l1 is%(equal-always) l2
    l1 isnot%(equal-always) l3
  end
}

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

@subsection[#:tag "s:always-equal-functions"]{Always Equal and Functions}

  When comparing two functions or two methods, @code{equal-always} raises an
  exception.  Why?  Well the traditional way to compare functions for equality
  (short of solving the halting problem), is to use reference equality on the
  function's representations.  For a hint of why this can be a misleading
  definition of equality, consider this data definition:

@pyret-block{
  data Stream<a>:
    | stream(first :: a, rest :: (-> Stream<a>))
  end
}

  And some tests:

@pyret-block{
  check:
    fun mk-ones():
      stream(1, ones)
    end
    ones = mk-ones()
    ones is%(equal-always) ones # Should this succeed?
    ones is%(equal-always) mk-ones() # What about this?
    ones.rest() is%(equal-always) mk-ones() # Or this...?
  end
}

  All of these values (@code{ones}, @code{mk-ones()}, etc.) have the same
  behavior, so we could argue that @code{is} (which uses @code{==} behind the
  scenes) ought to succeed on these.  And indeed, if we used reference
  equality, it would succeed.  But consider this small tweak to the program:
  
@pyret-block{
  check:
    fun mk-ones():
      stream(1, lam(): ones() end)  # <-- changed this line
    end
    ones = mk-ones()
    ones is%(equal-always) ones # Should this succeed?
    ones is%(equal-always) mk-ones() # What about this?
    ones.rest() is%(equal-always) mk-ones() # Or this...?
  end
}

  If we used reference equality on these functions, all of these tests would
  now fail, and @code{ones} @emph{has the exact same behavior}.  Here's the
  situation:

  When reference equality returns @code{true}, we know that the two functions
  must have the same behavior.  But when it returns @code{false}, we know
  nothing!  They functions may behave exactly the same, or they might be
  compeltely different, and the equality predicate can't tell us either way
  (indeed, this gets into the halting problem again).

  Pyret takes the following stance: You probably should rethink your program
  if it relies on comparing functions for equality, since Pyret cannot give
  reliable answers (no language can).  So, all the examples above actually
  raise exceptions:

@pyret-block{
  check:
    fun mk-ones():
      stream(1, lam(): ones() end)  # <-- changed this line
    end
    ones = mk-ones()
    ones == ones raises "Incomparable"
    ones == mk-ones() raises "Incomparable"
    ones.rest() == mk-ones() raises "Incomparable"
  end
}

  @bold{Note 1}: Functions can be compared with non-function values and return
  @code{false}.  That is, @code{equal-always} only throws the error if actual
  function values need to be compare to one another, not if a function value
  is compared to another type of value:

@pyret-block{
  check:
    f = lam(): "no-op" end
    g = lam(): "also no-op" end

    f == f raises "Incomparable"
    f == g raises "Incomparable"
    g == f raises "Incomparable"

    5 isnot%(equal-always) f

    { x: 5 } isnot%(equal-always) { x: f }
  end
}

  @bold{Note 2}: This rule about functions interacts with structural equality.
  When comparing two values, it seems at first unclear whether the result
  should be @code{false} or an error for this test:

@pyret-block{
  check:
    { x: 5, f: lam(): "no-op" end } is%(equal-always)
      { x: 6, f: lam(): "no-op" end }
  end
}

  This comparison will return @code{false}.  The rule is that if the equality
  algorithm can find values that differ without comparing functions, it will
  report the difference and return @code{false}.  However, if all of the
  non-function comparisons are @code{true}, then an error is raised.  A few
  more examples:

@pyret-block{

  check:
    o = { x: 5, y: { z: 6 }, lam(): "no-op" end }
    o2 = { x: 5, y: { z: 7 }, lam(): "no-op" end }

    (o == o) raises "Incomparable"
    o isnot%(equal-always) o2  # Test succeeds, because z fields differ
  end

}


@section{Equal Now}
  @function["equal-now" #:contract (a-arrow A A B)]

  Checks if the two values are equal @emph{now} (they may not be later).
  Corresponds to the @equal-now-op operator.

  @code{equal-now} only checks for equality @emph{now} because it traverses
  mutable values and checks their contents for equality.
  
@subsection{Equal Now and Primitives}
  
  Equal Now has the same behavior on primitives as Always Equal
  (@secref["s:always-equal-primitives"]).
  
@subsection[#:tag "s:equal-now-structural"]{Equal Now and Structural and Mutable Equality}

  Equal now has the same structural behavior as Always Equal
  (@secref["s:always-equal-structural"]) with the exception of mutable data.

  Instead of using Identical to check unfrozen references, Equal Now checks
  the contents of @emph{all} mutable data it reaches.  This gives it its name:
  since it only checks the @emph{current} values, and those fields might
  change, it is not true that if @code{e1 =~ e2}, then later @code{e1 =~ e2}
  will hold again.  For example:

@pyret-block{
  check:
    data MyBox:
      | my-box(ref x)
    end

    check:
      b1 = my-box(1)
      b2 = my-box(1)

      b1 is(equal-now) b2 is true
      b1!{x : 2}

      b1 is(equal-now) b2 is false
    end
  end
}

@subsection[#:tag "s:equal-now-functions"]{Equal Now and Functions}

  Equal now has the same behavior on functions as Always Equal
  (@secref["s:always-equal-structural"]).

@section{Identical}

  @function["identical" #:contract (a-arrow A A B)]


@subsection[#:tag "s:identical-primitives"]{Identical and Primitives}

  Identical has the same behavior on primitives as Always Equal
  (@secref["s:always-equal-primitives"]).

@subsection[#:tag "s:identical-structural"]{Identical and Structural Equality}

  Identical differs from the other equality operators in that it does not
  visit members of objects or data instances.  Instead, it checks if the
  values are actually the same exact value (the operator is meant to indicate
  that the values are interchangable).  So objects with the same fields are
  not identical:

@pyret-block{
  check:
    o = { x: 5 }
    o2 = { x: 5 }
    o isnot%(identical) o2
    o is%(identical) o
    o2 is%(identical) o
  end
}

@subsection[#:tag "s:identical-mutable"]{Identical and Mutable Data}

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

@subsection[#:tag "s:identical-functions"]{Identical and Functions}

  Equal Now has the same behavior on functions as Always Equal
  (@secref["s:always-equal-functions"]).


@section[#:tag "s:total-equality-predicates"]{Total Equality Functions (Avoiding Incomparability Errors)}

Most Pyret programs should be written using @code{equal-always},
@code{equal-now}, and @code{identical}, which guarantee that an error will be
raised if functions are compared.  Some programs, however, need to be able to
compare arbitrary values, and it's convenient to have the ability to compare
values without raising an exception.  Since the equality of functions is
unknown, we define the result of a total equality check with a new datatype:

@data-spec["Truth"]
@pyret-block{
  data Truth:
    | true
    | false
    | unknown
  end
}

@(define T (a-id "Truth" (xref "<equality>" "Truth")))

We define three parallel functions to the equality predicates that return
@code{Truth} values.  They return @code{true} and @code{false} whenever the
corresponding function would, and @code{unknown} whenever the corresponding
function would throw an error:

  @function["equal-always-3" #:contract (a-arrow A A T)]
  @function["equal-now-3" #:contract (a-arrow A A T)]
  @function["identical-3" #:contract (a-arrow A A T)]

For example:

@pyret-block{
  check:
    f = lam(): 5 end
    equal-always-3(f, f) is unknown
    equal-always-3(f, 5) is false
    equal-now-3(f, f) is unknown
    equal-now-3("a", f) is false
    identical-3("a", f) is unknown
    identical-3(f, f) is unknown
    identical-3("a", f) is false
  end
}

@section[#:tag "s:datatype-defined-equality"]{Datatype-defined Equality}

[FILL] Call the @code{_equals} method and stuff
