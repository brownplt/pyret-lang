#lang scribble/manual

@(require
  racket/list
  "common.rkt")

@title[#:tag "s:arrays"]{Arrays}

Pyret arrays are built-in, mutable, fixed-length, and indexed by non-negative
integers starting at 0.  Like Lists, in the current incarnation of Pyret they
are heterogeneous, though you can write a parameterized annotation for them
(e.g.  @code{Array<Number>}).

Two arrays are @code{==} (and as a consequence, can be compared with
@code{is}) if and only if they are @code{identical}.  To compare them for
equality in testing, the @code{array-to-list} function or @code{to-list}
methods are useful as they take an immutable snapshot of the @code{Array}'s
contents.

@section[#:tag "s:array-constr"]{Constructing Arrays}

@(define array-constructors
"
fun array(l :: List<a>) -> Array<a>:
  doc: 'Create a new array with the elements of l'
end
fun array-of(v :: a, n :: Number) -> Array<a>:
  doc: 'Create a new array of length n with all elements initially set to v'
end
fun <a> build-array(f :: (Number -> a), n :: Number) -> Array<a>:
  doc: 'Call f with numbers from 0 to n, creating an array from the results'
end
")
@(define array-constructors-ast (parse-pyret array-constructors))

@(pretty-functions array-constructors-ast '(array array-of build-array))

@section[#:tag "s:array-access"]{Manipulating Arrays}

There are a few constructs for working with arrays.  Array indexing is 0-based,
so the first element is at position 0.

@(define array-accessors
"
fun array-get(v :: Array<a>, n :: Number) -> a:
  doc: 'Get element at position n'
end
fun array-set(v :: Array<a>, n :: Number, v :: a) -> Array<a>:
  doc: 'Mutably update the element at position n.  Returns the updated array.'
end
fun array-length(v :: Array<a>) -> Number:
  doc: 'Return the number of elements in the array'
end
fun array-to-list(v :: Array<a>) -> List<a>:
  doc: 'Return a list with the elements of v in the same order'
end
")
@(define array-accessors-ast (parse-pyret array-accessors))

@(pretty-functions array-accessors-ast '(array-get array-set array-length array-to-list))


@section[#:tag "s:array-methods"]{Array Methods}

@(label "Array.get")

@justcode{
get(self :: Array<a>, n :: Number) -> a

check:
  a = array([1,2])
  a.get(0) is 1
  a.get(1) is 2
end
}

Accesses the item at position @code{n}. Method version of @code{array-get}.

@(label "Array.set")

@justcode{
set(self :: Array<a>, n :: Number, v :: a) -> Array<a>

check:
  a = array([1,2])
  a.get(0) is 1
  a.set(0, 42) is a
  a.get(0) is 42
end
}

Mutably updates the item at position @code{n}.  Method version of @code{array-set}.

@(label "Array.length")

@justcode{
length(self :: Array<a>) -> Number

check:
  a = array([1,2])
  a.get(0) is 1
  a.set(0, 42) is a
  a.get(0) is 42
end
}

Return the number of elements in the array.  Method version of @code{array-length}.

@(label "Array.to-list")

@justcode{
length(self :: Array<a>) -> List<a>

check:
  a = array([1,2])
  a.to-list() is [1,2]
end
}

Return the number of elements in the array.  Method version of @code{array-to-list}.
