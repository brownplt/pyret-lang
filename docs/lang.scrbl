#lang scribble/manual

@(require
  racket/list
  "common.rkt")
@(require
  (only-in racket collection-path)
  racket/list
  racket/file
  scribble/core
  scribble/manual
  (only-in racket/string string-join)
  pyret/lang/load
  pyret/lang/runtime
  pyret/lang/ffi-helpers
  pyret/lang/ast
  pyret/lang/desugar-check
  racket/match
  (rename-in "renderer.arr" (%PYRET-PROVIDE renderer)))

@title{Pyret Language Reference}

This document has detailed information on the Pyret grammar and the behavior of
its expression forms.  The entire grammar is included at the end of the
document for reference, with its pieces introduced individually first.  This
document cannot be read beginning-to-end assuming no experience with Pyret.
Rather, it explains in detail the semantics and grammar of each syntactic form,
and is heavily cross-referential when describing features that interact.  This
document also occasionally references ``Captain Teach'', which is a programming
and learning environment that uses Pyret, and has some of its own environmental
behavior that is worth noting.

@include-section{forms.scrbl}

@include-section{testing.scrbl}

@include-section{list.scrbl}


@section[#:tag "s:option"]{Option}

@subsection[#:tag "s:option-data"]{@tt{Option}}

@(define option (get-decl moorings-ast 'Option))

@(label-data option)
@(pretty-data option)

@(label "Option.orelse()")
@(pretty-method (get-method option 'none "orelse"))



@section[#:tag "s:set"]{Sets}

@subsection[#:tag "s:set-data"]{@tt{Set}}

Construct sets using @tt{sets.set(lst)}.
Sets have the type @tt{sets.Set}.

@margin-note{In Captain Teach and in @tt{#lang pyret/whalesong}, @tt{set}
and @tt{Set} are in the environment by default, so you can just use their
names as identifiers.}

@justcode{data Set: | ... end}

@(define set (get-decl moorings-ast 'Set))

@(define set-constructor-code
  "fun set(lst :: List):
     doc: 'Construct a set from a list.'
   end
")

@(define set-constructors (parse-pyret set-constructor-code))

@(pretty-functions set-constructors
   '(set))

@(label "Set.member()")
@(pretty-method-with-doc (get-method set '__set "member"))

@(label "Set.add()")
@(pretty-method-with-doc (get-method set '__set "add"))

@(label "Set.remove()")
@(pretty-method-with-doc (get-method set '__set "remove"))

@(label "Set.to-list()")
@(pretty-method-with-doc (get-method set '__set "to-list"))

@(label "Set.union()")
@(pretty-method-with-doc (get-method set '__set "union"))


@section[#:tag "s:numbers"]{Numbers}

@(define numbers
  "data Number:
    | num with:
      tostring(self) -> String: end,
      modulo(self) -> Number: end,
      truncate(self) -> Number: end,
      sin(self) -> Number: end,
      cos(self) -> Number: end,
      tan(self) -> Number: end,
      asin(self) -> Number: end,
      acos(self) -> Number: end,
      atan(self) -> Number: end,
      sqrt(self) -> Number: end,
      floor(self) -> Number: end,
      ceiling(self) -> Number: end,
      abs(self) -> Number: end,
      min(self, other :: Number) -> Number: end,
      max(self, other :: Number) -> Number: end,
      log(self) -> Number: 
        doc: 'The natural logarithm of this number'
      end,
      exp(self) -> Number:
        doc: \"Euler's constant raised to this number\"
      end,
      expt(self, power :: Number) -> Number:
        doc: 'This number raised to the specified power'
      end,
      exact(self) -> Number:
        doc: 'Some functions and methods, like substring, require exact numbers as arguments.  This method returns an exact version of this number suitable for such functions'
      end
  end")
@(define numbers-ast (parse-pyret numbers))

Numbers have a number of useful methods:

@(flatten (for/list ((method (get-all-methods (get-decl numbers-ast 'Number) 'num)))
  (list
    (label (string-append "Number." (get-method-name method)))
    (pretty-method method)
    (para (s-method-field-doc method)))))

@section[#:tag "s:strings"]{Strings}

@(define strings
  "data String:
    | str with:
      append(self, other :: String) -> String:
        doc: 'Append other after this string.'
      where:
        'hello '.append('world') is 'hello world'
      end,
      contains(self, other :: String) -> Bool:
        doc: 'Return true if other is contained in this string, false otherwise'
      where:
        'ahoy, matey'.contains('matey') is true
      end,
      substring(self, start :: Number, stop :: Number) -> String:
        doc: 'Return the substring of this string starting at index start and ending at index (stop - 1)'
      where:
        'a-str'.substring(0, 0) is ''
        'a-str'.substring(2, 5) is 'str'
      end,
      char-at(self, index :: Number) -> String:
        doc: 'Return the character at index as a string of length 1'
      where:
        'a'.char-at(0) is 'a'
        'ahoy'.char-at(3) is 'y'
      end,
      repeat(self, reps :: Number) -> Number:
        doc: 'Return a string that is this string repeated reps times'
      where:
        'yohoho'.repeat(3) is 'yohohoyohohoyohoho'
        ''.repeat(10) is ''
      end,
      length(self) -> Number:
        ''.length() is 0
        'yar'.length() is 3
      end,
      tonumber(self) -> Number:
        doc: 'Return this string parsed as a number.  Returns the special nothing value if the string is not a valid number'
      where:
        '5'.tonumber() is 5
        is-nothing('not-a-number'.tonumber()) is true
      end,
      tostring(self) -> String:
        doc: 'Returns this string'
      end
  end")
@(define strings-ast (parse-pyret strings))

@(flatten (for/list ((name '(
  "append"
  "contains"
  "substring"
  "char-at"
  "repeat"
  "length"
  "tonumber"
  "tostring"
)))
  (define pystr (get-decl strings-ast 'String))
  (list
    (label (string-append "String." name))
    (pretty-method (get-method pystr 'str name)))))

@section[#:tag "s:misc"]{Miscellaneous Functions}

@(define misc
  "fun gensym(prefix :: String) -> String:
    doc: 'Generate a random string with the given prefix'
  end
  fun raise(value :: Any) -> Nothing:
    doc: 'Raises the given value, to be caught by except, raises, or to show an error to the user'
  end
  fun print(value :: Any) -> Nothing:
    doc: 'Prints the given value as a string'
  end
  fun torepr(value :: Any) -> String:
    doc: 'Returns a string that resembles the expression used to construct value.  Useful for REPL printing.'
  end
  fun read-sexpr(str :: String) -> Any:
    doc: 'Take a string as input, and parse it into an s-expression.
Each s-expression is a number, symbol, string, or a list of
s-expressions surrounded by parenthesis and separated by whitespace.
Parenthesized lists are converted into Pyret lists, and strings
are converted into a list [\"string\", <the-string>].'
  where:
      read-sexpr('((-13 +14 88.8) cats ++ \"dogs\")')
    is [[-13, 14, 88.8], 'cats', '++', ['string', 'dogs']]
  end
  fun random(n :: Number) -> Number:
    doc: 'Take a number as input, and return a random number between 0 and n-1 (inclusive'
  end
  fun is-number(v :: Any) -> Bool:
    doc: 'True if v is a number, false otherwise'
  end
  fun is-string(v :: Any) -> Bool:
    doc: 'True if v is a string, false otherwise'
  end
  fun is-object(v :: Any) -> Bool:
    doc: 'True if v is a object, false otherwise'
  end
  fun is-bool(v :: Any) -> Bool:
    doc: 'True if v is a bool, false otherwise'
  end
  fun is-function(v :: Any) -> Bool:
    doc: 'True if v is a function, false otherwise'
  end
  fun is-method(v :: Any) -> Bool:
    doc: 'True if v is a method, false otherwise'
  end
  fun is-mutable(v :: Any) -> Bool:
    doc: 'True if v is a mutable, false otherwise'
  end
  fun is-placeholder(v :: Any) -> Bool:
    doc: 'True if v is a placeholder, false otherwise'
  end
  fun is-nothing(v :: Any) -> Bool:
    doc: 'True if v is nothing, false otherwise'
  end"
  )
@(define misc-ast (parse-pyret misc))

@(pretty-functions misc-ast '(
  gensym
  raise
  print
  torepr
  read-sexpr
  is-number
  is-string
  is-bool
  is-function
  is-method
  is-object
  is-mutable
  is-placeholder
  is-nothing
  ))

@section[#:tag "s:mutables"]{Mutables}

A @tt{Mutable} value is created implicitly by use of the @tt{mutable} keyword
on a field, or explicitly with the @tt{mk-mutable} function.  Most Pyret
programs do not need to create @tt{Mutable}s explicitly.

@(label "mk-mutable")

@justcode{
<a> mk-mutable(
      val :: a,
      read :: (a -> Bool),
      write :: (a -> Bool)
    )
    -> Mutable<a>
}

This creates a mutable value holding @tt{val} as a value, and with predicates
@tt{read} and @tt{write} checked on access and update, respectively.  These
predicates are automatically filled in based on annotations for @tt{Mutable}s
created by @tt{data}.  If the predicate fails in either case, a type error is
signalled.

Aside from constructing a @tt{Mutable} directly, they can be accessed directly
by using a @seclink["s:colon-expr" "colon expression"], which gives access to
several methods:

@(label "Mutable.get")

@justcode{
get(self :: Mutable<a>) -> a
}

Returns the value in the @tt{Mutable}.  

@(label "Mutable._equals")

@justcode{
get(self :: Mutable<a>, other :: Any) -> Bool
}

Returns @tt{true} if @tt{other} is the @emph{same} mutable as this one,
@tt{false} otherwise.

@(label "Mutable.tostring")

@justcode{
tostring(self :: Mutable<a>) -> String
}

Returns a string representation of the mutable.  To avoid cyclic printing, this
just gives the string @tt{"mutable-field"}.

@section[#:tag "s:placeholders"]{Placeholders}

Placeholder values are created automatically by @tt{graph} declarations, and
can be explicitly created with @tt{mk-placeholder}, though most Pyret programs
will not need to.  This section is mainly for those wishing a deeper
understanding of how @tt{graph} and @tt{cyclic} interact.

A @tt{Placeholder} is a value that supports a single @emph{update}, and
multiple @emph{guards} before being updated.  The design goal of
@tt{Placeholder}s is to support many contexts sharing the same (possibly
un-initialized) value, with each context placing its own constraints on the
value prior to it being initialized.  The following methods are on each
@tt{Placeholder} value:

@(label "Placeholder.get")

@justcode{
get(self :: Placeholder<a>) -> a
}

Get the value in the placeholder.  Signals an exception if the value has yet to
be initialized with the @tt{set} method.

@(label "Placeholder.guard")

@justcode{
guard(self :: Placeholder<a>, guard :: (a -> Bool)) -> Nothing
}

Update the placeholder with an additional check @tt{guard}.  Signals an
exception if the value has yet to be initialized with the @tt{set} method.

@(label "Placeholder.set")

@justcode{
set(self :: Placeholder<a>, value :: a) -> a
}

Initialize the placeholder with @tt{value}.  Signals an exception if any of the
guards on the placeholder return non-@tt{true} values when applied to
@tt{value}, or if the placeholder has already been @tt{set}. 

These three methods are used in concert by @tt{graph} and @tt{cyclic} to safely
and lazily intialize mutually-referential data.  Each left-hand side in a
@tt{graph} binding gets a new placeholder value with no guards.  Each
constructor with a @tt{cyclic} modifier on a field that is called adds a guard
to the placeholder based on the annotation on that field.  Then, at the end of
the @tt{graph} declaration, the placeholders are all @tt{set} to the values on
the right-hand side of the graph bindings.  If any placeholder was used in an
incorrect context, a type error is signalled at this point.  After graph
initialization, subsequent accesses of the placeholder fields with
@seclink["s:dot-expr" "dot expressions"] use the @tt{get} operation to access
the initialized value.

A few other methods are on @tt{Placeholder} values, as well:

@(label "Placeholder._equals")

@justcode{
get(self :: Placeholder<a>, other :: Any) -> Bool
}

Returns @tt{true} if @tt{other} is the @emph{same} @tt{Placeholer} as this one,
@tt{false} otherwise.

@(label "Placeholder.tostring")

@justcode{
tostring(self :: Placeholder<a>) -> String
}

Returns a string representation of the placeholder.  To avoid cyclic printing, this
just gives the string @tt{"placeholder-field"}.

@include-section{ast.scrbl}
