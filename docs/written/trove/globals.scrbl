#lang scribble/base
@(require "../../scribble-api.rkt" "../abbrevs.rkt")

@(append-gen-docs
'(module
  "<global>"
  (path "src/js/base/runtime-anf.js")
  (data-spec
    (name "Any")
    (variants)
    (shared))
  (data-spec
    (name "Number")
    (variants)
    (shared))
  (data-spec
    (name "Boolean")
    (variants)
    (shared))
  (data-spec
    (name "String")
    (variants)
    (shared))
  (data-spec
    (name "Nothing")
    (variants)
    (shared))
  (data-spec
    (name "Function")
    (variants)
    (shared))
  (data-spec
    (name "RawArray")
    (variants)
    (shared))
  (data-spec
    (name "Method")
    (variants)
    (shared))
  (data-spec
    (name "Object")
    (variants)
    (shared))
  (fun-spec
    (name "raise")
    (arity 1)
    (args ("val"))
    (return (a-id "Nothing" (xref "<global>" "Nothing")))
    (doc "Raises the value as an exception"))
  (fun-spec
    (name "torepr")
    (arity 1)
    (args ("val"))
    (return (a-id "String" (xref "<global>" "String")))
    (doc "Creates a string representation of the value"))
  (fun-spec
    (name "tostring")
    (arity 1)
    (args ("val"))
    (return (a-id "String" (xref "<global>" "String")))
    (doc "Creates a string representation of the value"))
  (fun-spec
    (name "is-boolean")
    (arity 1)
    (args ("val"))
    (doc "Checks whether the provided argument is in fact a boolean"))
  (fun-spec
    (name "is-number")
    (arity 1)
    (args ("val"))
    (doc "Checks whether the provided argument is in fact a number"))
  (fun-spec
    (name "is-string")
    (arity 1)
    (args ("val"))
    (doc "Checks whether the provided argument is in fact a string"))
  (fun-spec
    (name "is-nothing")
    (arity 1)
    (args ("val"))
    (doc "Checks whether the provided argument is in fact nothing"))
  (fun-spec
    (name "is-function")
    (arity 1)
    (args ("val"))
    (doc "Checks whether the provided argument is in fact a function"))
  (fun-spec
    (name "is-object")
    (arity 1)
    (args ("val"))
    (doc "Checks whether the provided argument is in fact an object"))
  (fun-spec
    (name "is-method")
    (arity 1)
    (args ("val"))
    (doc "Checks whether the provided argument is in fact a method"))
  (fun-spec
    (name "is-raw-array")
    (arity 1)
    (args ("val"))
    (doc "Checks whether the provided argument is in fact a raw array"))

))

@docmodule["<global>" #:noimport #t #:friendly-title "Global Utilities"]{

@section[#:tag "global-builtins"]{Built-in Utility Functions}

@function["torepr" #:contract (a-arrow A S) #:alt-docstrings ""]

Creates a string representation of the value that resembles an expression that
could be used to construct it.  This is what the REPL and test-results printer
use to display values. 

@examples{
check:
  # torepr wraps strings in quotes
  torepr("this-will-have-quotes") is "\"this-will-have-quotes\""

  # torepr on lists always prints with [list: ...] notation
  torepr(link(1, empty)) is "[list: 1]"
}

@function["tostring" #:contract (a-arrow A S) #:alt-docstrings ""]

Creates a string representation of the value for display, that is
value-dependent.  Error messages, for example, have different
@pyret-id{tostring} results that print line and column information and a
formatted error message, which is much different from their @pyret-id{torepr}.

@function["raise" #:contract (a-arrow A No) #:alt-docstrings ""]

Raises the value as an error.  This usually stops the program and reports the
raised value, but errors can be caught and checked in tests by
@pyret-id["raises" "testing"] and by @seclink["testing-blocks"]{@pyret{check:}
blocks}.




@section{Built-in Types}

@type-spec["Any" (list)]

A type specification that permits all values.  This is mainly useful
in built-in language forms, like in @secref["equality"] or
@pyret-id{torepr}, which truly do handle any value.  Pyret programs that
use @pyret-id{Any} on their own can usually be restructured to not use the annotation
and be clearer about what data they are working with.

@type-spec["Number" (list)]

The type of @seclink["numbers"].

@type-spec["Boolean" (list)]

The type of @seclink["booleans"].

@type-spec["String" (list)]

The type of @seclink["strings"].

@type-spec["RawArray" (list)]

The type of @seclink["raw-arrays"].

@type-spec["Nothing" (list)]

The type of the special value @pyret{nothing}, used in contexts where the
program evaluates but has no meaningful answer by design (see, for example
@pyret-id["each" "lists"]).

@type-spec["Object" (list)]

The type of all values constructed from @pyret{data} constructors and
singletons, and by object literals.

@type-spec["Function" (list)]

The type of all function values.

@type-spec["Method" (list)]

The type of all method values; most Pyret programs should never need to work
with method values directly.

@section{Type Predicates}

A number of functions are available to tell which kind of builtin value a
particular value is.

@function["is-boolean" #:contract (a-arrow "Any" (a-id "Boolean" (xref "<global>" "Boolean")))]

Returns @tt{true} for @tt{true} and @tt{false}, and @tt{false} for all other values.

@function["is-string" #:contract (a-arrow "Any" (a-id "Boolean" (xref "<global>" "Boolean")))]

  @para{Returns true for strings, false for non-strings.  Strings can be written @tt{@literal{"}text@literal{"}} or @tt{@literal{'}text@literal{'}}, 
  and may not span multiple lines.  Allowed escapes are @tt{\n} (newline), 
  @tt{\r} (carriage return), @tt{\t} (tab), @tt{\[0-8]{1,3}} for octal escapes, 
  @tt{\x[0-9a-fA-F]{1,2}} for single-byte hexadecimal escapes, or @tt{\u[0-9a-fA-F]{1,4}} 
  for double-byte Unicode escapes.  Additionally, @tt{@literal{\"}} escapes a double-quote within a 
  double-quoted string, and @tt{@literal{\'}} escapes a single quote within a single-quoted string.}
  
  @para{Multi-line string literals may be written @tt{@literal{```} text @literal{```}}.  The same escape sequences
  are valid as for single-line strings.  Leading and trailing whitespace of the string are
  trimmed.}

@function["is-number" #:contract (a-arrow "Any" (a-id "Boolean" (xref "<global>" "Boolean")))]

  Returns true for numbers, false for non-numbers.  Numbers are @itemlist[
     @item{Integers, e.g. @tt{345} or @tt{-321}}
     @item{Rationals, e.g. @tt{355/113} or @tt{-321/6789}}
     @item{Inexact numbers, e.g. @tt{123.4567} or @tt{-0.987}}
     @item{Complex numbers, e.g. @tt{1+2i}, where the real and imaginary components may be integers, rationals or inexact numbers}
  ]

@function["is-function" #:contract (a-arrow "Any" (a-id "Boolean" (xref "<global>" "Boolean")))]

Returns true for functions, false for non-functions

@function["is-nothing" #:contract (a-arrow "Any" (a-id "Boolean" (xref "<global>" "Boolean")))]

@function["is-object" #:contract (a-arrow "Any" (a-id "Boolean" (xref "<global>" "Boolean")))]

@function["is-method" #:contract (a-arrow "Any" (a-id "Boolean" (xref "<global>" "Boolean")))]

@function["is-raw-array" #:contract (a-arrow "Any" (a-id "Boolean" (xref "<global>" "Boolean")))]




   
}
