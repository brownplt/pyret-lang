#lang scribble/manual

@(require
  racket/list
  "common.rkt")

@title[#:tag "s:vectors"]{Vectors}

Pyret vectors are built-in, mutable, fixed-length, and indexed by non-negative
integers starting at 0.  Like Lists, in the current incarnation of Pyret they
are heterogeneous, though you can write a parameterized annotation for them
(e.g.  @code{Vector<Number>}).

Two vectors are @code{==} (and as a consequence, can be compared with
@code{is}) if and only if they are @code{identical}.  To compare them for
equality in testing, the @code{vector-to-list} function or @code{to-list}
methods are useful as they take an immutable snapshot of the @code{Vector}'s
contents.

@section[#:tag "s:vector-constr"]{Constructing Vectors}

@(define vector-constructors
"
fun vector(l :: List<a>) -> Vector<a>:
  doc: 'Create a new vector with the elements of l'
end
fun const-vector(v :: a, n :: Number) -> Vector<a>:
  doc: 'Create a new vector of length n with all elements initially set to v'
end
")
@(define vector-constructors-ast (parse-pyret vector-constructors))

@(pretty-functions vector-constructors-ast '(vector const-vector))

@section[#:tag "s:vector-access"]{Manipulating Vectors}

There are a few constructs for working with vectors:

@(define vector-accessors
"
fun vector-get(v :: Vector<a>, n :: Number) -> a:
  doc: 'Get element at position n'
end
fun vector-set(v :: Vector<a>, n :: Number, v :: a) -> Vector<a>:
  doc: 'Mutably update the element at position n.  Returns the updated vector.'
end
fun vector-length(v :: Vector<a>) -> Number:
  doc: 'Return the number of elements in the vector'
end
fun is-vector(v) -> Number:
  doc: 'Return true if v is a Vector, false otherwise'
end
fun vector-to-list(v :: Vector<a>) -> List<a>:
  doc: 'Return a list with the elements of v in the same order'
end
")
@(define vector-accessors-ast (parse-pyret vector-accessors))

@(pretty-functions vector-accessors-ast '(vector-get vector-set vector-length is-vector vector-to-list))


@section[#:tag "s:vector-methods"]{Vector Methods}

@(label "Vector.get")

@justcode{
get(self :: Vector<a>, n :: Number) -> a

check:
  v = vector([1,2])
  v.get(0) is 1
  v.get(1) is 2
end
}

Accesses the item at position @code{n}. Method version of @code{vector-get}.

@(label "Vector.set")

@justcode{
set(self :: Vector<a>, n :: Number, v :: a) -> Vector<a>

check:
  v = vector([1,2])
  v.get(0) is 1
  v.set(0, 42) is v
  v.get(0) is 42
end
}

Mutably updates the item at position @code{n}.  Method version of @code{vector-set}.

@(label "Vector.length")

@justcode{
length(self :: Vector<a>) -> Number

check:
  v = vector([1,2])
  v.get(0) is 1
  v.set(0, 42) is v
  v.get(0) is 42
end
}

Return the number of elements in the vector.  Method version of @code{vector-length}.

@(label "Vector.to-list")

@justcode{
length(self :: Vector<a>) -> List<a>

check:
  v = vector([1,2])
  v.to-list() is [1,2]
end
}

Return the number of elements in the vector.  Method version of @code{vector-to-list}.
