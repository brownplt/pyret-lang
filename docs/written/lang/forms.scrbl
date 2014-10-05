#lang scribble/base

@(require
  racket/list
  racket/file
  (only-in racket/string string-join)
  "../../scribble-api.rkt")

@(define (prod . word)
 (apply tt word))
@(define (file . name)
 (apply tt name))
@(define (in-code . code)
 (apply tt code))
@(define (justcode . stx)
 (nested #:style 'code-inset
  (verbatim (string-join stx ""))))

@title[#:tag "s:forms" #:style '(toc)]{Language Constructs}

This section contains information on the various language forms in Pyret, from
binary operators to data definitions to functions.  This is a more detailed
reference to the grammar of expressions and statements and their evaluation,
rather than to 

@(table-of-contents)

@section[#:tag "s:program"]{Programs}

Programs consist of a sequence of import or provide statements, followed by a
block:

@justcode{
program: prelude block
prelude: [provide-stmt] [provide-types-stmt] import-stmt*

provide-stmt: PROVIDE stmt end | PROVIDE STAR
provide-types-stmt: PROVIDE-TYPES record-ann | PROVIDE-TYPES STAR
}

@section{Import Statements}

Import statements come in a few forms:

@justcode{
import-stmt: IMPORT import-source AS NAME
import-stmt: IMPORT NAME (COMMA NAME)* FROM import-source
import-source: import-special | import-name | import-string 
import-special: NAME PARENNOSPACE STRING (COMMA STRING)* RPAREN
import-name: NAME
import-string: STRING
}


The form with @justcode{import-name} looks for a file with that name in the
built-in libraries of Pyret, and it is an error if there is no such library.

Example:

@pyret{
  import equality as EQ
  check:
    f = lam(): "" end
    EQ.equal-always3(f, f) is EQ.Unknown
  end
}



@section{Provide Statements}

A provide statement comes in one of two forms:

@justcode{
provide-stmt: "provide" stmt "end" | "provide" "*"
}

Both forms have no effect when the program is run as the top-level program.

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

Where the @justcode{id}s are all the toplevel names in the file defined with
@pyret{fun}, @pyret{data}, or @pyret{x = e}.

@section{Blocks}

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
handled specially and non-locally within blocks.  A detailed description of
scope will appear here soon.

Blocks evaluate each of their statements in order, and evaluate to the value of
the final statement in the block.

@section{Statements}

There are a number of forms that can only appear as statements in @tt{block}s
and @tt{provide} expressions:

@justcode{
stmt: let-expr | fun-expr | data-expr | when-expr
    | var-expr | assign-expr | binop-expr
}

@subsection[#:tag "s:let-expr"]{Let Expressions}

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

@subsection[#:tag "s:fun-expr"]{Function Declaration Expressions}

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
f = lam(x, y):
  x + y
end
}

See the documentation for @tt{lambda-exprs} for an explanation of arguments'
and annotations' behavior, as well as @tt{doc-strings}.

@subsection[#:tag "s:data-expr"]{Data Declarations}

Data declarations define a number of related functions for creating and
manipulating a data type.  Their grammar is:

@justcode{
data-expr: "data" NAME ty-params data-mixins ":"
    data-variant*
    data-sharing
    where-clause
  "end"
data-mixins: ["deriving" mixins]
data-variant: "|" NAME variant-members data-with | "|" NAME data-with
variant-members: (PARENSPACE|PARENNOSPACE) [list-variant-member* variant-member] ")"
list-variant-member: variant-member ","
variant-member: ["mutable"|"cyclic"] binding
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
@seclink["s:dot-expr" "dot expressions"].

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
  a-btree = node(1, leaf(2), node(3, leaf(4), leaf(2)))
  a-btree.values-equal(leaf(1)) is true
  leaf(1).values-equal(a-btree) is true
  a-btree.size() is 3
  leaf(0).size() is 1
  leaf(1).increment() is leaf(2)
  a-btree.increment() # raises error: field increment not found.
end
}



@subsection[#:tag "s:var-expr"]{Variable Declarations}

Variable declarations look like @seclink["s:let-expr" "let bindings"], but
with an extra @tt{var} keyword in the beginning:

@justcode{
var-expr: "var" binding "=" expr
}

A @tt{var} expression creates a new @emph{assignable variable} in the current
scope, initialized to the value of the expression on the right of the @tt{=}.
It can be accessed simply by using the variable name, which will always
evaluate to the last-assigned value of the variable.  @seclink["s:assign-expr"
"Assignment statements"] can be used to update the value stored in an
assignable variable.

If the @tt{binding} contains an annotation, the initial value is checked
against the annotation, and all @seclink["s:assign-expr" "assignment
statements"] to the variable check the annotation on the new value before
updating.

@subsection[#:tag "s:assign-expr"]{Assignment Statements}

Assignment statements have a name on the left, and an expression on the right
of @tt{:=}:

@justcode{
assign-expr: NAME ":=" binop-expr
}

If @tt{NAME} is not declared in the same or an outer scope of the assignment
expression with a @tt{var} declaration, the program fails with a static error.

At runtime, an assignment expression changes the value of the assignable
variable @tt{NAME} to the result of the right-hand side expression.

@section{Expressions}

@subsection[#:tag "s:lam-expr"]{Lambda Expressions}

The grammar for a lambda expression is:

@justcode{
lambda-expr: "lam" ty-params [args] return-ann ":"
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
add1 = lam(x :: Number):
  x + 1
end
add1("not-a-number")
# Error: expected a Number and got "not-a-number"
}


@subsection[#:tag "s:app-expr"]{Application Expressions}

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

@subsection[#:tag "s:curried-apply-expr"]{Curried Application Expressions}

Suppose a function is defined with multiple arguments:

@justcode{
fun f(v, w, x, y, z): ... end
}

Sometimes, it is particularly convenient to define a new function that
calls @tt{f} with some arguments pre-specified:

@justcode{
call-f-with-123 = lam(y, z): f(1, 2, 3, y, z) end
}

Pyret provides syntactic sugar to make writing such helper functions
easier:

@justcode{
call-f-with-123 = f(1, 2, 3, _, _) # same as the fun expression above
}

Specifically, when Pyret code contains a function application some of
whose arguments are underscores, it constructs an anonymous function
with the same number of arguments as there were underscores in the
original expression, whose body is simply the original function
application, with the underscores replaced by the names of the
arguments to the anonymous function.

This syntactic sugar also works
with operators.  For example, the following are two ways to sum a list
of numbers:

@justcode{
[list: 1, 2, 3, 4].foldl(lam(a, b): a + b end, 0)

[list: 1, 2, 3, 4].foldl(_ + _, 0)
}

Likewise, the following are two ways to compare two lists for
equality:

@justcode{
list.map_2(lam(x, y): x == y end, first-list, second-list)

list.map_2(_ == _, first-list, second-list)
}

Note that there are some limitations to this syntactic sugar.  You
cannot use it with the @tt{is} or @tt{raises} expressions in
check blocks, since both test expressions and expected
outcomes are known when writing tests.  Also, note that the sugar is
applied only to one function application at a time.  As a result, the
following code:

@justcode{
_ + _ + _
}

desugars to

@justcode{
lam(z):
  (fun (x, y): x + y end) + z
end
}

which is probably not what was intended.  You can still write the
intended expression manually:

@justcode{
lam(x, y, z): x + y + z end
}

Pyret just does not provide syntactic sugar to help in this case
(or other more complicated ones).

@subsection[#:tag "s:cannonball-expr"]{Chaining Application}

@justcode{
CARET: "^"
chain-app-expr: binop-expr CARET binop-expr
}

The expression @pyret{e1 ^ e2} is equivalent to @pyret{e2(e1)}.  It's just
another way of writing a function application to a single argument.

Sometimes, composing functions doesn't produce readable code.  For example, if
say we have a @pyret{Tree} datatype, and we have an @pyret{add} operation on
it, defined via a function.  To build up a tree with a series of adds, we'd
write something like:

@pyret-block{
t = add(add(add(add(empty-tree, 1), 2), 3), 4)
}

Or maybe

@pyret-block{
t1 = add(empty-tree, 1)
t2 = add(t1, 2)
t3 = add(t2, 3)
t  = add(t3, 4)
}

If @pyret{add} were a method, we could write:

@pyret-block{
t = empty-tree.add(1).add(2).add(3).add(4)
}

which would be more readable, but since @pyret{add} is a function, this doesn't
work.

In this case, we can write instead:

@pyret-block{
t = empty-tree ^ add(_, 1) ^ add(_, 2) ^ add(_, 3)
}

This uses @seclink["s:curried-apply-expr" "curried application"] to create a
single argument function, and chaining application to apply it.  This can be
more readable across several lines of initialization as well, when compared to
composing “inside-out” or using several intermediate names:

@pyret-block{
t = empty-tree
  ^ add(_, 1)
  ^ add(_, 2)
  ^ add(_, 3)
  # and so on
}

@subsection[#:tag "s:binop-expr"]{Binary Operators}

There are a number of binary operators in Pyret.  A binary operator expression
is written by putting an operator between two other expressions, as in:

@justcode{
binop-expr: binop-expr BINOP binop-expr
}

Each binary operator is syntactic sugar for a particular method or function
call.  The following table lists the operators, their intended use, and the
corresponding call:

@tabular[#:sep @hspace[2]
  (list
    (list @tt{left + right} @tt{left._plus(right)})
    (list @tt{left - right} @tt{left._minus(right)})
    (list @tt{left * right} @tt{left._times(right)})
    (list @tt{left / right} @tt{left._divide(right)})
    (list @tt{left <= right} @tt{left._lessequal(right)})
    (list @tt{left < right} @tt{left._lessthan(right)})
    (list @tt{left >= right} @tt{left._greaterequal(right)})
    (list @tt{left > right} @tt{left._greaterthan(right)}))
]

For the primitive strings and numbers, the operation happens internally.  For
all object values, the operator looks for the method appropriate method and
calls it.  The special names allow a form of operator overloading, and avoid
adding an extra concept beyond function and method calls to the core to
account for these binary operations.

@subsection[#:tag "s:obj-expr"]{Object Expressions}

Object expressions map field names to values:

@justcode{
obj-expr: "{" fields "}" | "{" "}"
fields: list-field* field [","]
list-field: field ","
field: key ":" binop-expr
     | key args return-ann ":" doc-string block where-clause "end"
key: NAME
}


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

@subsection[#:tag "s:dot-expr"]{Dot Expressions}

A dot expression is any expression, followed by a dot and name:

@justcode{
dot-expr: expr "." NAME
}

A dot expression evaluates the @tt{expr} to a value @tt{val}, and then does one
of five things:

@itemlist[
  @item{Raises an exception, if @tt{NAME} is not a field of @tt{expr}}

  @item{Evaluates to the value stored in @tt{NAME}, if @tt{NAME} is present and
  not a method}

  @item{
  
    If the @tt{NAME} field is a method value, evaluates to a function that is
    the @emph{method binding} of the method value to @tt{val}.  For a method 
    
    @justcode{
      m = method(self, x): body end
    }

    The @emph{method binding} of @tt{m} to a value @tt{v} is equivalent to:

    @justcode{
      (lam(self): lam(x): body end end)(v)
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

@subsection[#:tag "s:extend-expr"]{Extend Expressions}

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

@subsection[#:tag "s:if-expr"]{If Expressions}

An if expression has a number of test conditions and an optional else case.

@justcode{
if-expr: IF binop-expr COLON block else-if* [ELSECOLON block] end
else-if: ELSEIF binop-expr COLON block
}

For example, this if expression has an "else:"

@pyret-block{
if x == 0:
  1
else if x > 0:
  x
else:
  x * -1
end
}

This one does not:

@pyret-block{
if x == 0:
  1
else if x > 0:
  x
end
}

Both are valid.  The conditions are tried in order, and the block corresponding
to the first one to return @pyret{true} is evaluated.  If no condition matches,
the else branch is evaluated if present.  If no condition matches and no else
branch is present, an error is thrown.  If a condition evaluates to a value
other than @pyret{true} or @pyret{false}, a runtime error is thrown.

@subsection[#:tag "s:ask-expr"]{Ask Expressions}

An @pyret{ask} expression is a different way of writing an @pyret{if}
expression that can be easier to read in some cases.

@justcode{
ask-expr: ASKCOLON ask-branch* [BAR OTHERWISECOLON block] end
ask-branch: BAR binop-expr THENCOLON block
}

This ask expression:

@pyret-block{
ask:
  | x == 0 then: 1
  | x > 0 then: x
  | otherwise: x * -1
end
}

is equivalent to

@pyret-block{
if x == 0:
  1
else if x > 0:
  x
else:
  x * -1
end
}

Similar to @pyret{if}, if an @pyret{otherwise:} branch isn't specified and no
branch matches, a runtime error results.

@subsection[#:tag "s:cases-expr"]{Cases Expressions}

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
  result = cases(list.List) [list: 1,2,3]:
    | empty => "empty"
    | link(f, r) => "link"
  end
  result is "link"

  result2 = cases(list.List) [list: 1,2,3]:
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

@subsection[#:tag "s:for-expr"]{For Expressions}

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
fun-expr(lam(arg1 :: ann1, ...) -> ann-return: block end, expr1, ...)
}

Using a @tt{for-expr} can be a more natural way to call, for example, list
iteration functions because it puts the identifier of the function and the
value it draws from closer to one another.  Use of @tt{for-expr} is a matter of
style; here is an example that compares @tt{fold} with and without @tt{for}:

@justcode{
for fold(sum from 0, number from [list: 1,2,3,4]):
  sum + number
end

fold(lam(sum, number): sum + number end, 0, [list: 1,2,3,4])
}

@section[#:tag "s:annotations"]{Annotations}

Annotations in Pyret express intended types values will have at runtime.
They appear next to identifiers anywhere a @tt{binding} is specified in the
grammar, and if an annotation is present adjacent to an identifier, the program
is compiled to raise an error if the value bound to that identifier would
behave in a way that violates the annotation.  The annotation provides a
@emph{guarantee} that either the value will behave in a particular way, or the
program will raise an exception.

@subsection[#:tag "s:name-ann"]{Name Annotations}

Some annotations are simply names.  For example, a
@seclink["s:data-expr"]{@tt{data declaration}} binds the name of the
declaration as a value suitable for use as a name annotation.  There are
built-in name annotations, too:

@justcode{
Any
Number
String
Boolean
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

@subsection[#:tag "s:arrow-ann"]{Arrow Annotations}

An arrow annotation is used to describe the behavior of functions.  It consists
of a list of comma-separated argument types followed by an ASCII arrow and
return type:

@justcode{
arrow-ann: (PARENSPACE|PARENNOSPACE) arrow-ann-elt* ann "->" ann ")"
arrow-ann-elt: ann ","
}

When an arrow annotation appears in a binding, that binding position simply
checks that the value is a function.

