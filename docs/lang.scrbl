#lang scribble/manual

@(require
  (only-in racket collection-path)
  racket/list
  racket/file
  scribble/core
  (only-in racket/string string-join)
  pyret/lang/load
  pyret/lang/runtime
  pyret/lang/ffi-helpers
  pyret/lang/ast
  racket/match
  (rename-in "renderer.arr" (%PYRET-PROVIDE renderer)))

@(define (pretty ast)
  (define lines 
    (ffi-unwrap ((p:p-base-app (p:get-raw-field p:dummy-loc renderer "get-pretty-str"))
          (p:p-opaque ast))))

  (apply joincode lines))
@(define (get-decl ast name)
  (define (name-matches? sym) (equal? sym name))
  (match ast
    [(s-prog _ _ (s-block _ stmts))
     (findf (lambda (s)
      (match s
        [(s-fun _ (? name-matches? test-name) _ _ _ _ _ _) s]
        [(s-data _ (? name-matches? test-name) _ _ _ _ _) s]
        [_ #f])) stmts)]))

@(define (label name)
  (toc-target-element #f @(bold name) (list (string->symbol name) name)))

@(define (pretty-fun fun)
  (match fun
    [(s-fun loc name params args ann doc _ check)
     (pretty (s-fun loc name params args ann "" (s-block loc '()) check))]))
@(define (label-fun fun (prefix ""))
  (match fun
    [(s-fun loc name params args ann doc _ check)
     (toc-target-element #f @(bold (string-append prefix (symbol->string name))) (list name (symbol->string name)))]))

@(define (pretty-data data)
  (define (simplify-variant v)
    (match v
      [(s-singleton-variant loc name with) (s-singleton-variant loc name empty)]
      [(s-variant loc name args with) (s-variant loc name args empty)]))
  (match data
    [(s-data loc name params mixins variants _ _)
     (pretty (s-data loc name params mixins (map simplify-variant variants) empty (s-block loc empty)))]))

@(define (get-all-methods data variant-name)
  (match data
    [(s-data loc name params mixins variants _ _)
     (define with-members (filter-map (lambda (v) (variant-matches v variant-name)) variants))
     (when (< (length with-members) 1)
      (error "No such variant: ~a\n" variant-name))
     (first with-members)]))
  
@(define (variant-matches v variant-name)
  (match v
    [(s-singleton-variant loc name with)
     (if (equal? name variant-name) with #f)]
    [(s-variant loc name args with)
     (if (equal? name variant-name) with #f)]
    [_ #f]))
@(define (get-method data variant-name method-name)
  (define (member-matches v)
    (match v
      [(s-method-field loc (s-str _ name) args ann doc body check)
       (if (equal? name method-name)
           (s-method loc args ann doc body check)
           #f)]
      [_ #f]))
  (match data
    [(s-data loc name params mixins variants _ _)
     (define with-members (filter-map (lambda (v) (variant-matches v variant-name)) variants))
     (when (< (length with-members) 1)
      (error "No such variant: ~a\n" variant-name))
     (define method-fields (filter-map member-matches (first with-members)))
     (when (< (length method-fields) 1)
      (error "No such field: ~a\n" method-name))
     (first method-fields)]))

@(define (pretty-method method-field)
  (match method-field
   [(s-method loc args ann doc body check)
    (pretty (s-method loc args ann "" (s-block loc empty) check))]
   [(s-method-field loc name args ann doc body check)
    (pretty (s-method-field loc name args ann "" (s-block loc empty) check))]
   [_ (error (format "Not a method: ~a\n" method-field))]))
  
@(define (label-data data (prefix ""))
  (match data
    [(s-data loc name _ _ _ _ _)
     (toc-target-element #f @(bold (string-append prefix (symbol->string name))) (list name (symbol->string name)))]))


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
and is heavily cross-referential when describing features that interact.  This
document also occasionally references ``Captain Teach'', which is a programming
and learning environment that uses Pyret, and has some of its own environmental
behavior that is worth noting.

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

@subsubsection[#:tag "s:let-expr"]{Let Expressions}

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

@subsubsection[#:tag "s:graph-expr"]{Graph Declarations}

Graph declarations look like a series of let statements:

@justcode{
graph-expr: "graph:" let-expr* "end"
}

They behave like let statements, binding variables in the block in which they
appear.  However, they allow for the creation of mutual references between
data.  For example:

@justcode{
check:
  graph:
    BOS = [PVD, WOR]
    WOR = [BOS]
    PVD = [BOS]
  end
  BOS.first is PVD
  BOS.rest.first is WOR
  WOR.first is BOS
  PVD.first is BOS
end
}

If we wrote this with a let expression, we would run in the gotcha about using
an identifier before we defined it.  But @tt{graph:} keeps track of these
mutual references and causes the right relationships to hold at the end of the
@tt{graph:} block.  Fields that use a graph identifier need to be declared as
@tt{cyclic} in their data declaration, otherwise an error results.


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

A function expression is syntactic sugar for a let and an anonymous function
expression.  The statement:

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
f = fun(x, y):
  x + y
end
}

See the documentation for @tt{lambda-exprs} for an explanation of arguments'
and annotations' behavior, as well as @tt{doc-strings}.

@subsubsection[#:tag "s:data-expr"]{Data Declarations}

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

Here is a longer example of the behavior of detectors, field access, and
constructors:

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

A data definition can also define, for each instance as well as for the data
definition as a whole, a set of methods.  This is done with the keywords
@tt{with:} and @tt{sharing:}.  Methods defined on a variant via @tt{with:} will
only be defined for instances of that variant, while methods defined on the
union of all the variants with @tt{sharing:} are defined on all instances.  For
example:

@justcode{
data BTree:
  | node(value :: Number, left :: BTree, right :: BTree) with:
    size(self): 1 + self.left.size() + self.right.size() end
  | leaf(value :: Number) with:
    size(self): 1 end,
    increment(self): leaf(self.value + 1) end
sharing:
  values-equal(self, other):
    self.value == other.value
  end
where:
  a-btree = node(1, leaf(2), node(3, leaf(4)))
  a-btree.values-equal(leaf(1)) is true
  leaf(1).values-equal(a-btree) is true
  a-btree.size() is 3
  leaf(0).size() is 1
  leaf(1).increment() is leaf(2)
  a-btree.increment() # raises error: field increment not found.
end
}

A data definition also sets up some special methods that are used by other
constructs in the language.  Most of the time, you shouldn't need to call these
directly, but they are present on each instance:

@itemlist[
  @item{@tt{tostring} is a method that produces a string
        representation of the value}

  @item{@tt{_torepr} is a method that produces a string that represents the
  value in ``constructor form''.  This is distinct from @tt{tostring} in that,
  for example, the @tt{tostring} of @tt{"a-str"} is the string value
  @tt{"a-str"}, but the @tt{_torepr} is @tt{"\"a-str\""}.  This produces more
  meaningful REPL output, among other things.}
 
  @item{@tt{_equals} is a method used to check equality with other values.  It
  is called implicitly by the @tt{==} operator.}

  @item{@tt{_match} is a method that is used by @seclink["s:cases-expr" "cases
  expressions"].}

]

@;{ TODO: singleton variants and mixins }

@subsubsection[#:tag "s:var-expr"]{Variable Declarations}

@subsubsection[#:tag "s:assign-expr"]{Assignment Statements}

@subsection{Expressions}

@subsubsection[#:tag "s:lam-expr"]{Lambda Expressions}

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
@seclink["s:app-expr" "application expressions"].  The arguments in @tt{args}
are bound to their arguments as immutable identifiers as in a
@seclink["s:let-expr" "let expression"].  These identifiers follow the same
rules of no shadowing and no assignment.

If the arguments have @seclink["s:annotations" "annotations"] associated with
them, they are checked before the body of the function starts evaluating, in
order from left to right.  If an annotation fails, an exception is thrown.

@justcode{
add1 = fun(x :: Number):
  x + 1
end
add1("not-a-number")
# Error: expected a Number and got "not-a-number"
}

Functions values are created with a @tt{"_doc"} field which holds the string
value of the @tt{doc-string} written in the function expression.  So:

@justcode{
documented = fun():
  doc: "Evaluates to a standards-compliant random number"
  4
end

check:
  documented._doc is "Evaluates to a standards-compliant random number"
end
}

@subsubsection[#:tag "s:app-expr"]{Application Expressions}

Function application expressions have the following grammar:

@justcode{
app-expr: expr app-args
app-args: PARENNOSPACE [app-arg-elt* binop-expr] ")"
app-arg-elt: binop-expr ","
}

An application expression is an expression (usually expected to evaluate to a
function), followed by a comma-separated list of arguments enclosed in
parentheses.  It first evaluates the arguments in left-to-right order, then
evaluates the function position.  If the function position is a function value,
the number of provided arguments is checked against the number of arguments
that the function expects.  If they match, the arguments names are bound to the
provided values.  If they don't, an exception is thrown.

Note that there is @emph{no space} allowed before the opening parenthesis of
the application.  If you make a mistake, Pyret will complain:

@justcode{
f(1) # This is the function application expression f(1)
f (1) # This is the id-expr f, followed by the paren-expr (1)
# The second form yields a well-formedness error that there
# are two expressions on the same line
}

@subsubsection[#:tag "s:left-apply-expr"]{Caret Application Expressions}

@margin-note{
The grammar of @tt{left-app-fun-expr} is restricted to avoid confusing
constructions, like:

@justcode{
obj^f(1)(2)
}

in which it would be unclear if the function to call is @tt{f} or @tt{f(1)}.}
An application can also be written with a caret symbol @tt{^}:

@justcode{
left-app-expr: expr "^" left-app-fun-expr app-args
left-app-fun-expr: id-expr | id-expr "." NAME
}

This is merely syntactic sugar for putting the initial @tt{expr} as the first
argument of an application to the @tt{left-app-fun-expr}.  These are equivalent:

@justcode{
obj^f(1, 2, 3)
}

@justcode{
f(obj, 1, 2, 3)
}

This allows for patterns like method-chaining on values that do not have
methods defined.  For example, one could write a function @tt{add-each} that
adds to each element of a list, and another function @tt{square} that
squares them, and chain them linearly:

@justcode{
check:
  [1,2,3]^inc(1)^square() is [4, 9, 16]
end
}



@subsubsection[#:tag "s:obj-expr"]{Object Expressions}

Object expressions map field names to values:

@justcode{
obj-expr: "{" fields "}" | "{" "}"
fields: list-field* field [","]
list-field: field ","
field: key ":" binop-expr
     | key args return-ann ":" doc-string block where-clause "end"
key: NAME | "[" binop-expr "]"
}

@margin-note{The ability to define fields as computed strings using
@tt{["brack" + "ets"]} is deprecated, and will be replaced in the future by
other reflective operations on objects and dictionaries.}

A comma-separated sequence of fields enclosed in @tt{{}} creates an object; we
refer to the expression as an @emph{object literal}.  There are two types of
fields: @emph{data} fields and @emph{method} fields.  A data field in an object
literal simply creates a field with that name on the resulting object, with its
value equal to the right-hand side of the field.  A method field

@justcode{
key args return-ann ":" doc-string block where-clause "end"
}

is syntactic sugar for:

@justcode{
key ":" "method" args return-ann ":" doc-string block where-clause "end"
}

That is, it's just special syntax for a data field that contains a method
value.

@margin-note{The overriding of later fields is expected to be deprecated and
replaced with an error.}

The fields are evaluated in order.  If the same field appears more than once,
the later use overrides the earlier use, but both field expressions are still
evaluated.

@subsubsection[#:tag "s:list-expr"]{List Expressions}

A list expression is a sequence of comma-separated expressions enclosed in
@tt{[]}:

@justcode{
list-elt: binop-expr ","
list-expr: "[" [list-elt* binop-expr] "]"
}

An empty list literal @tt{[]} is syntactic sugar for @tt{list.empty}.

A list with elements in it is transformed into a sequence of nested calls to
@tt{list.link}, ending in @tt{list.empty}.  For example:

@justcode{
[1,2,3]
}

becomes

@justcode{
list.link(1, list.link(2, list.link(3, list.empty)))
}

See the documentation for @seclink["s:lists" "lists"] for more information on
the values created by @tt{list.link} and @tt{list.empty}.

@subsubsection[#:tag "s:dot-expr"]{Dot Expressions}

A dot expression is any expression, followed by a dot and name:

@justcode{
dot-expr: expr "." NAME
}

A dot expression evaluates the @tt{expr} to a value @tt{val}, and then does one
of five things:

@itemlist[
  @item{Raises an exception, if @tt{NAME} is not a field of @tt{expr}}

  @item{Evaluates to the value stored in @tt{NAME}, if @tt{NAME} is present and
  not a method, mutable, or placeholder value}

  @item{Raises an exception, if the value stored in @tt{NAME} is a @tt{mutable} field.}

  @item{Evaluates to the value stored in the placeholder stored in @tt{NAME},
  if the value is a placeholder value.}

  @item{
  
    If the @tt{NAME} field is a method value, evaluates to a function that is
    the @emph{method binding} of the method value to @tt{val}.  For a method 
    
    @justcode{
      m = method(self, x): body end
    }

    The @emph{method binding} of @tt{m} to a value @tt{v} is equivalent to:

    @justcode{
      (fun(self): fun(x): body end end)(v)
    }

    What this detail means is that you can look up a method and it
    automatically closes over the value on the left-hand side of the dot.  This
    bound method can be freely used as a function.

    For example:

    @justcode{
      o = { m(self, x): self.y + x end, y: 22 }
      check:
        the-m-method-closed-over-o = o.m
        m(5) is 27
      end
    }
  }
]

@subsubsection[#:tag "s:colon-expr"]{Colon Expressions}

The colon expression is like the dot expression, but does not perform method
binding.  It is written as a dot expression, but with @tt{:} rather than @tt{.}.

@justcode{
colon-expr: expr ":" NAME
}

@subsubsection[#:tag "s:extend-expr"]{Extend Expressions}

The extend expression consists of an base expression and a list of fields to
extend it with:

@justcode{
extend-expr: expr "." "{" fields "}"
}

The extend expression first evaluates @tt{expr} to a value @tt{val}, and then
creates a new object with all the fields of @tt{val} and @tt{fields}.  If a
field is present in both, the new field is used.

Examples:

@justcode{
check:
  o = {x : "original-x", y: "original-y"}
  o2 = o.{x : "new-x", z : "new-z"}
  o2.x is "new-x"
  o2.y is "original-y"
  o2.z is "new-z"
end
}

@subsubsection[#:tag "s:if-expr"]{If Expressions}

@subsubsection[#:tag "s:cases-expr"]{Cases Expressions}

A cases expression consists of a datatype (in parentheses), an expression to
inspect (before the colon), and a number of branches.  It is intended to be
used in a structure parallel to a data definition.

@justcode{
cases-expr: "cases" (PARENSPACE|PARENNOSPACE) expr-check ")" expr-target ":"
    cases-branch*
    ["|" "else" "=>" block]
  "end"
cases-branch: "|" NAME [args] "=>" block
}

A @tt{cases} expression first evaluates @tt{expr-check} to get a checker for
the type of the value to branch on.  Typically, this should be the name of a
datatype like @tt{list.List}.  The expression then evaluates @tt{expr-target},
and checks if it matches the given annotation.  If it does not, an exception is
raise, otherwise it proceeds to match it against the given cases.

@margin-note{ Under the hood, @tt{cases} is calling the @tt{_match} function of
the target value, which is defined for each variant and performs the
appropriate dispatch.}

Cases should use the names of the variants of the given data type as the
@tt{NAME}s of each branch.  The branches will be tried, in order, checking if
the given value is an instance of that variant.  If it matches, the fields of
the variant are bound, in order, to the provided @tt{args}, and the right-hand
side of the @tt{=>} is evaluated in that extended environment.  An exception
results if the wrong number of arguments are given.

An optional @tt{else} clause can be provided, which is evaluated if no cases
match.  If no @tt{else} clause is provided, a default is used that raises an
exception.

For example, a cases expression on lists looks like:

@justcode{
check:
  result = cases(list.List) [1,2,3]:
    | empty => "empty"
    | link(f, r) => "link"
  end
  result is "link

  result2 = cases(list.List) [1,2,3]:
    | empty => "empty"
    | else => "else"
  end
  result2 is else

  result3 = cases(list.List) empty:
    | empty => "empty"
    | else => "else"
  end
  result3 is "empty"
end
}

@subsubsection[#:tag "s:for-expr"]{For Expressions}

For expressions consist of the @tt{for} keyword, followed by a list of
@tt{binding from expr} clauses in parentheses, followed by a block:

@justcode{
for-expr: "for" expr PARENNOSPACE [for-bind-elt* for-bind] ")" return-ann ":"
  block
"end"
for-bind-elt: for-bind ","
for-bind: binding "from" binop-expr
}

The for expression is just syntactic sugar for a
@seclink["s:lam-expr"]{@tt{lam-expr}} and a @seclink["s:app-expr"]{@tt{app-expr}}.  An expression

@justcode{
for fun-expr(arg1 :: ann1 from expr1, ...) -> ann-return:
  block
end
}

is equivalent to:

@justcode{
fun-expr(fun(arg1 :: ann1, ...) -> ann-return: block end, expr1, ...)
}

Using a @tt{for-expr} can be a more natural way to call, for example, list
iteration functions because it puts the identifier of the function and the
value it draws from closer to one another.  Use of @tt{for-expr} is a matter of
style; here is an example that compares @tt{fold} with and without @tt{for}:

@justcode{
for fold(sum from 0, number from [1,2,3,4]):
  sum + number
end

fold(fun(sum, number): sum + number end, [1,2,3,4])
}

@subsubsection[#:tag "s:try-expr"]{Try Expressions}



@subsection[#:tag "s:annotations"]{Annotations}

Annotations in Pyret express intended types values will have at runtime.
They appear next to identifiers anywhere a @tt{binding} is specified in the
grammar, and if an annotation is present adjacent to an identifier, the program
is compiled to raise an error if the value bound to that identifier would
behave in a way that violates the annotation.  The annotation provides a
@emph{guarantee} that either the value will behave in a particular way, or the
program will raise an exception.

@subsubsection[#:tag "s:name-ann"]{Name Annotations}

Some annotations are simply names.  For example, a
@seclink["s:data-expr"]{@tt{data declaration}} binds the name of the
declaration as a value suitable for use as a name annotation.  There are
built-in name annotations, too:

@justcode{
Any
Number
String
Bool
}

Each of these names represents a particular type of runtime value, and using
them in annotation position will check each time the identifier is bound that
the value is of the right type.

@justcode{
x :: Number = "not-a-number"
# Error: expected Number and got "not-a-number"
}

@tt{Any} is an annotation that allows any value to be used.  It semantically
equivalent to not putting an annotation on an identifier, but it allows a
program to clearly signal that no restrictions are intended for the identifier
it annotates.

@subsubsection[#:tag "s:arrow-ann"]{Arrow Annotations}

An arrow annotation is used to describe the behavior of functions.  It consists
of a list of comma-separated argument types followed by an ASCII arrow and
return type:

@justcode{
arrow-ann: (PARENSPACE|PARENNOSPACE) arrow-ann-elt* ann "->" ann ")"
arrow-ann-elt: ann ","
}

When an arrow annotation appears in a binding, that binding position
@emph{wraps} values that are bound to it in a new function.  This new function,
when applied, checks that the arguments match the provided list of argument
annotations, and if any fail, raises an exception.  When (or if) the function
finishes evaluating to a value, it checks that the resulting value matches the
@emph{return annotation}, which appears after the arrow, again signalling an
exception if it does not.

@justcode{
# This line does not cause an error yet
f :: (Number -> String) = fun(x): x + 1 end

# This raises an exception "Expected Number, got 'not-a-number'"
f("not-a-number")

# This raises an exception "Expected String, got 4"
f(3)
}

@subsection{Complete Grammar}

@(apply joincode (rest (file->lines (collection-file-path "lang/grammar.rkt" "pyret"))))


@section[#:tag "s:lists"]{Lists}

@subsection[#:tag "s:lists-data"]{@tt{List}}

@subsubsection{The List Datatype}

Pyret lists are defined via a data declaration:

@(label-data (get-decl moorings-ast 'List))
@(pretty-data (get-decl moorings-ast 'List))

@subsubsection{List Methods}

@tt{List} instances have a number of methods.  The following methods are
defined on all lists (instances of @tt{empty} and @tt{link}):

@(flatten (for/list ((name '(
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
)))
  (define pylist (get-decl moorings-ast 'List))
  (list
    (label (string-append "List." name))
    (pretty-method (get-method pylist 'link name))
    (para (s-method-doc (get-method pylist 'link name))))))

@subsection[#:tag "s:lists-functions"]{@tt{list} functions}

@margin-note{In Captain Teach and in @tt{#lang pyret/whalesong}, these are in
the environment by default, so you can just use their names as identifiers.}

All of the functions in this section are available on the @tt{list} object, and
help manipulate lists.

@(define moorings-ast (parse-pyret (file->string (collection-file-path "lang/pyret-lib/moorings.arr" "pyret"))))

@(define (pretty-functions block names)
  (flatten (for/list ((name names))
    (define f (get-decl block name))
    (list (label-fun f)
          (nested (pretty-fun f)
                  (para (s-fun-doc f)))))))
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
 ))

@section[#:tag "s:option"]{Option}

@subsection[#:tag "s:option-data"]{@tt{Option}}

@(define option (get-decl moorings-ast 'Option))

@(label-data option)
@(pretty-data option)

@(label "Option.orelse()")
@(pretty-method (get-method option 'none "orelse"))

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

@(define (get-method-name m)
  (match m
    [(s-method-field l (s-str _ name) _ _ _ _ _) name]))

@(flatten (for/list ((method (get-all-methods (get-decl numbers-ast 'Number) 'num)))
  (list
    (label (string-append "Number." (get-method-name method)))
    (pretty-method method)
    (para (s-method-field-doc method)))))

@section[#:tag "s:strings"]{Strings}

@(define strings
  "data String:
    | str with:
      append(self, other :: String) -> String: end,
      contains(self, other :: String) -> Bool: end,
      substring(self, start :: Number, stop :: Number) -> String: end,
      char-at(self, index :: Number) -> String: end,
      repeat(self, reps :: Number) -> Number: end,
      length(self) -> Number: end,
      tonumber(self) -> Number: end,
      tostring(self) -> String: end
  end")
@(define strings-ast (parse-pyret strings))

Strings have a number methods:

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
  end"
  )
@(define misc-ast (parse-pyret misc))

@(pretty-functions misc-ast '(
  gensym
  raise
  print
  torepr
  read-sexpr
  ))

