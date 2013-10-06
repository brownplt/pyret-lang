#lang scribble/manual

@(require
  racket/list
  "common.rkt")

@title[#:tag "s:lists"]{Lists}

Pyret lists are defined by a Pyret data structure, and come with a number of
built-in functions and methods.

@section[#:tag "s:lists-data"]{@tt{List}}

@subsection{The List Datatype}

Pyret lists are defined via a data declaration:

@(label-data (get-decl moorings-ast 'List))
@(pretty-data (get-decl moorings-ast 'List))

@subsection{List Methods}

@tt{List} instances have a number of methods.  The following methods are
defined on all lists (instances of @tt{empty} and @tt{link}):

@(define pylist (get-decl moorings-ast 'List))
@(method-doc-list pylist "List." 'link '(
    "length"
    "each"
    "map"
    "filter"
    "find"
    "partition"
    "foldr"
    "foldl"
    "member"
    "append"
    "last"
    "take"
    "drop"
    "reverse"
    "get"
    "set"
    "_equals"
    "tostring"
    "_torepr"
    "sort-by"
    "sort"
    "join-str"
))

@section[#:tag "s:lists-functions"]{@tt{list} functions}

@margin-note{In Captain Teach and in @tt{#lang pyret/whalesong}, these are in
the environment by default, so you can just use their names as identifiers.}

All of the functions in this section are available on the @tt{list} object, and
help manipulate lists.

@(pretty-functions moorings-ast '(
    range
    repeat
    filter
    partition
    any
    find
    map
    map2
    map3
    map4
    map_n
    map2_n
    map3_n
    map4_n
    each
    each2
    each3
    each4
    each_n
    each2_n
    each3_n
    each4_n
    fold
    fold2
    fold3
    fold4
    index
 ))



