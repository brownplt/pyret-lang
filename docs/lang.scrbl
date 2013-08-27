#lang scribble/manual

@(require
  (only-in racket collection-path)
  racket/list
  racket/file
  scribble/core
  (only-in racket/string string-join))


@(define (joincode . stx)
  (nested #:style 'code-inset
   (verbatim (string-join stx "\n"))))
@(define (justcode . stx)
  (nested #:style 'code-inset
   (verbatim (string-join stx ""))))


@title{Pyret Language Reference}

This document has detailed information on the Pyret grammar and the behavior of
its expression forms.  The entire grammar is included at the end of the
document for reference, with its pieces introduced individually first.  This
document cannot be read beginning-to-end assuming no experience with Pyret.
Rather, it explains in detail the semantics and grammar of each syntactic form,
and is heavily cross-referential when describing features that interact.

@section{Language Constructs}

@subsection{Programs}

Programs consist of a sequence of import or provide statements, followed by a
block:

@justcode{
program: prelude block
prelude: (import-stmt|provide-stmt)*
block: stmt*
}

@subsection{Import Statements}

@margin-note{Import and provide statements aren't available in Captain Teach;
the page is responsible for providing all the needed libraries.}

Import statements come in two forms:

@justcode{
import-stmt: "import" (import-name | import-string) "as" NAME
import-name: NAME
import-string: STRING
}

Both forms bind the value provided by the target (either @tt{import-name} or
@tt{import-string}) to the @tt{NAME} after @tt{as}.

The form with @tt{STRING} as a target transforms the import into a Racket require
statement, using the string as the module path.  For example, given this
@tt{"m.arr"}:

@justcode{
  provide m end
  m = 22
}

Another file in the same directory could use it:

@justcode{
  import "m.arr" as m
  print(m) # prints 22
}

It is an error if the file does not exist, or does not have a provide
statement.

The form that uses a @tt{NAME} production looks for a file with that name in the
built-in libraries of Pyret.  These are currently found in the @tt{lang/racket-ffi/}
directory of Pyret, and are maintained by the Pyret authors.

Example:

@justcode{
  import io as IO
  IO.read-line()
}

It is an error if there is no such named file in @tt{lang/racket-ffi/}, or if the
file does not provide an identifier named @tt{%PYRET-PROVIDE}.

@subsection{Provide Statements}

A provide statement comes in one of two forms:

@justcode{
provide-stmt: "provide" stmt "end" | "provide" "*"
}

Both forms have no effect when the program is run as the top-level program
(e.g. in a Captain Teach editor, DrRacket buffer, or as the target of @tt{raco
pyret}).

When the program is in a file that is evaluated via @tt{import}, the program is
run, and then the @tt{provide} statement is run in top-level scope to determine
the value bound to the identifier in the import statement.

In the first form, the @tt{stmt} internal to the provide is evaluated, and the
resulting value is provided.

The second form is syntactic sugar for:

@justcode{
provide {
  id: id,
  ...
} end
}


@subsection{Blocks}

A block's syntax is a list of statements:

@justcode{
block: stmt*
}

Blocks serve two roles in Pyret:

@itemlist[
  @item{Sequencing of operations}
  @item{Units of lexical scope}
]

The @tt{let-expr}, @tt{fun-expr}, @tt{data-expr}, and @tt{var-expr} forms are
handled specially and non-locally within blocks.  They all define names that
are in scope for all expressions in the block.

Example:

@justcode{
  fun f(): x end # x is defined when f evaluates
  x = 12
  f() # evaluates to 12
}

Example:

@justcode{
  data Node:
    | node(in :: Edge, out :: Edge) # Edge is visible
  end

  data Edge:
    | edge(weight :: Number)
  end
}

There is one gotcha, which is that if identifiers are evaluated before they are
defined, as opposed to just being closed over, Pyret throws an exception:

@justcode{
  x = y
  y = 9
}

(Note: Right now this program runs but it shouldn't.  The value of @tt{x} is
@tt{#<undefined>}, which is an issue with a designed but un-implemented fix.)

The sections on the individual statements describe which names they introduce
into the scope.

Blocks evaluate each of their statements in order, and evaluate to the value of
the final statement in the block.

@subsection{Statements}

There are a number of forms that can only appear as statements in @tt{block}s
and @tt{provide} expressions:

@justcode{
stmt: let-expr | fun-expr | data-expr | when-expr
    | var-expr | assign-expr | binop-expr
}

@subsubsection{Let Expressions}

Let expressions are written with an equals sign:

@justcode{
let-expr: binding "=" binop-expr
}

A let statement causes the name in the @tt{binding} to be put in scope in the
current block, and upon evaluation sets the value to be the result of
evaluating the @tt{binop-expr}.  The resulting binding cannot be changed via an
@tt{assign-expr}, and cannot be shadowed by other bindings within the same or
nested scopes:

@justcode{
x = 5
x := 10
# Error: x is not assignable

}

@justcode{
x = 5
x = 10
# Error: x defined twice

}

@justcode{
x = 5
fun f():
  x = 10
end
# Error: can't use the name x in two nested scopes

}

@justcode{
fun f():
  x = 10
end
fun g():
  x = 22
end
# Not an error: x is used in two scopes that are not nested
}


@subsubsection[#:tag "s:fun-expr"]{Function Declaration Expressions}

Function declarations have a number of pieces:

@justcode{
fun-expr: "fun" fun-header ":" doc-string block where-clause "end"
fun-header: ty-params NAME args return-ann
ty-params:
  ["<" list-ty-param* NAME ">"]
list-ty-param: NAME ","
args: (PARENSPACE|PARENNOSPACE) [list-arg-elt* binding] ")"
list-arg-elt: binding ","
return-ann: ["->" ann]
doc-string: ["doc:" STRING]
where-clause: ["where:" block]
}

A function expression is syntactic sugar for a let and a lambda expression.
The statement:

@justcode{
"fun" ty-params NAME args return-ann ":"
  doc-string
  block
  where-clause
"end"
}

Is equivalent to

@justcode{
NAME "=" "fun" ty-params args return-ann ":"
  doc-string
  block
"end"
}

With the @tt{where-clause} registered in check mode.  Concretely:

@justcode{
fun f(x, y):
  x + y
end
}

is equivalent to

@justcode{
fun f(x, y):
  x + y
end
}

See the documentation for @tt{lambda-exprs} for an explanation of arguments'
and annotations' behavior, as well as @tt{doc-strings}.

@subsubsection{Data Declarations}

Data declarations define a number of related functions for creating and
manipulating a data type.  Their grammar is:

@justcode{
data-expr: "data" NAME ty-params data-mixins ":"
    data-variant*
    data-sharing
    where-clause
  "end"
data-mixins: ["deriving" mixins]
data-variant: "|" NAME args data-with | "|" NAME data-with
data-with: ["with:" fields]
data-sharing: ["sharing:" fields]
}

A @tt{data-expr} causes a number of new names to be bound in the scope of the
block it is defined in:

@itemlist[
  @item{The @tt{NAME} of the data definition}
  @item{@tt{NAME}, for each variant of the data definition}
  @item{@tt{is-NAME}, for each variant of the data definition}
]

For example, in this data definition:

@justcode{
data BTree:
  | node(value :: Number, left :: BTree, right :: BTree)
  | leaf(value :: Number)
end
}

These names are defined, with the given types:

@justcode{
BTree :: (Any -> Bool)
node :: (Number, BTree, BTree -> BTree)
is-node :: (Any -> Bool)
leaf :: (Number -> BTree)
is-leaf :: (Any -> Bool)
}

We call @tt{node} and @tt{leaf} the @emph{constructors} of @tt{BTree}, and they
construct values with the named fields.  They will refuse to create the value
if fields that don't match the annotations are given.  As with all annotations,
they are optional.  The constructed values can have their fields accessed with
@seclink["s:dot-expr" "dot expressions"] and @seclink["s:colon-expr" "colon
expressions"].

The function @tt{BTree} is a @emph{detector} for values created from this data
definition, and can be used as an annotation to check for values created by the
constructors of @tt{BTree}.  @tt{BTree} returns true when provided values
created by @tt{node} or @tt{leaf}, but no others.

The functions @tt{is-node} and @tt{is-leaf} are detectors for the values
created by the individual constructors: @tt{is-node} will only return @tt{true}
for values created by calling @tt{node}, and correspondingly for @tt{leaf}.

@justcode{
data BTree:
  | node(value :: Number, left :: BTree, right :: BTree)
  | leaf(value :: Number)
where:
  a-btree = node(1, leaf(2), node(3, leaf(4), leaf(5)))

  BTree(a-btree) is true
  BTree("not-a-tree") is false
  BTree(leaf(5)) is false
  is-leaf(leaf(5)) is true
  is-leaf(a-btree) is false
  is-leaf("not-a-tree") is false
  is-node(leaf(5)) is false
  is-node(a-btree) is true
  is-node("not-a-tree") is false

  a-btree.value is 1
  a-btree.left.value is 2
  a-btree.right.value is 3
  a-btree.right.left.value is 4
  a-btree.right.right.value is 4

end
}


In the first case of @tt{data-variant}, the individual variants' @tt{NAME}s are
bound to functions with the same arguments as in the @tt{args} list after the
name (if args are given)

@justcode{
data BTree:
  | node(value :: Number, left :: BTree, right :: BTree) with
    size(self): 1 + self.left.size() + self.right.size() end
  | leaf
    size(self): 0 end
sharing
  
end
}

@subsection{Expressions}

@subsubsection{Lambda Expressions}

The grammar for a lambda expression is:

@justcode{
lambda-expr: "fun" ty-params [args] return-ann ":"
    doc-string
    block
    where-clause
  "end"
fun-header: ty-params NAME args return-ann
ty-params:
  ["<" list-ty-param* NAME ">"]
list-ty-param: NAME ","
args: (PARENSPACE|PARENNOSPACE) [list-arg-elt* binding] ")"
list-arg-elt: binding ","
return-ann: ["->" ann]
doc-string: ["doc:" STRING]
}

@margin-note{
The @tt{ty-params} and @tt{where-clause} of lambda expressions are currently not
interpreted by Pyret.  The @tt{ty-params} will be used when Pyret has more
complete support for checking polymorphic functions.  The @tt{where-clause} is
included for homogeneity with @seclink["s:fun-expr" "function statements"]. 
}

A lambda expression creates a function value that can be applied with
@seclink["s:apply-expr" "application expressions"].  The arguments in @tt{args}
are bound to their arguments as immutable identifiers as in a
@seclink["s:let-expr" "let expression"].  These identifiers follow the same
rules of no shadowing and no assignment.

If the arguments have @seclink["s:annotations" "annotations"] associated with
them, they are checked before the body of the function starts evaluating, in
order from left to right.  If an annotation fails, an exception is thrown.

@justcode{
fun add1(x :: Number):
  x + 1
end
f("not-a-number")
# Error: expected a Number and got "not-a-number"
}

@subsubsection[#:tag "s:apply-expr"]{Application Expressions}

@subsection[#:tag "s:annotations"]{Annotations}


@subsection{Complete Grammar}

@(apply joincode (rest (file->lines (collection-file-path "lang/grammar.rkt" "pyret"))))


@section{Libraries}

@toc-target-element[#f "list.foldr" (list 'list-foldr "list.foldr")]

