# A Tour of the Language

Once you've gotten started by [installing
Pyret](/getting-started/), you can start playing with it.  Copy
the examples below, run them, and play with them to get a feel for
the language.

## Testing and Assertions

Pyret has built-in support for testing, and tries to encourage it
as much as possible.  In fact, by default, Pyret will complain
when you run a program that _doesn't_ define any tests.  If you just run a program like

    #lang pyret/check
    "Ahoy world!"

Pyret will dutifully print the value, and then complain:

    "Ahoy world!"
    
    WARNING: Your program didn't define any tests.  Add some

To make this warning go away, you need to add at least one test.
The simplest way to add a test to a Pyret program is to use a
`check:` block and a testing assertion.  Try running the following:

    check:
      "Ahoy " + "world!" is "Ahoy world!"
    end

Running this will give you:

    Looks shipshape, your 1 test passed, mate!

What this program does is use a `check:` block to register a set of tests to be
run.  The special `is` statement inside the check block compares the
expressions on the left and right for equality.  It reports the result to the
built-in testing framework, which produces a report when all the tests have
been run.

If we break the test slightly, we can see that Pyret reports the
error for us:

    check:
      "Ahoy" + "world!" is "Ahoy world!"
    end

    In check block at /home/joe/scratch/examples.arr: line 4, column 0
    In test at /home/joe/scratch/examples.arr: line 5, column 2
    Test "Ahoy" + "world!" is "Ahoy world!" failed:
    Values not equal: 
    "Ahoyworld!"
    
    "Ahoy world!"
    
    Avast, there be bugs!
    Total: 1, Passed: 0, Failed: 1, Errors in tests: 0, Errors in between tests: 0

The usual flow of writing a Pyret program involves writing tests along with
your code, running your code to check the test output, and repeating.  The more
tests you write, the more useful feedback you get.

The examples in this tour will all be presented in testing blocks
(you'll see one kind other than `check:` later).  Unless we're
explicitly pointing out a failure, you can assume that the tests
all pass, and we're showing correct behavior.


## Primitive Values and Operators

Primitives values are the basic building blocks of the language;
structured data exists to organize computation over a small set of
primitive values.  We describe Pyret's primitives here.

### Numbers

Numbers can be written with or without decimals.  For example:

    check:
      5.0 is 5
    end

Pyret defines a number of binary operators over numbers (the full list is
available in [the documentation](/docs/s_forms.html#%28part._s~3abinop-expr%29)):

    check:
      4 + 5 is 9
      1 / 3 is 2 / 6
      9 - 3 is 6
      5 > 4 is true
    end

Once we have binary operators, it is natural to ask what operator
precedence Pyret has chosen.  In order to avoid ambiguity and
confusing updates to precedence tables when new operators are
added, chains of operators in Pyret simply disallows mixing
operators without disambiguating parentheses.  For example:

    check:
      5 - 4 + 1 is 2
    end

    
    well-formedness: Cannot mix binary operators of different
    types: `+` and `-`. Use parentheses to disambiguate.

This holds not just for numeric operations, but for all binary
operators in the language.  To give this program the meaning
intended by the test, we should write:

    check:
      (5 - 4) + 1 is 2
    end


### Strings

Strings can be written single- or double- quoted.  They may span
multiple lines. Character escapes like \n work, and the enclosing
quote character can be included if escaped like \".

    "hello world"

    "hello out
     there"

    "\"yields falsehood when...\" yields falsehood when"

    "an example\n\nwith explicit newline\ncharacters"

### Lists

Lists aren't quite primitive values, since they are a form of
structured data, but they _are_ built in to Pyret.  Lists group a
sequence of values together.  They are most easily written as a
comma-separated list of values enclosed in square brackets.  The
elements of a list can be accessed through the dot lookup
expression, via the members called called `first` and `rest`:

    check:
      [1,2,3].first is 1
      [1,2,3].rest  is [2,3]
    end

It is an error to access a field that isn't there; for example
trying to access `first` on a list with no elements.  We can use
the `raises` test assertion to check the error that's signalled:

    check:
      [].first raises "first was not found on []"
    end

The `raises` form is useful for checking explicit error conditions.  It
succeeds if the expression on the left signals an error with a message that
contains the string on the right.

The `[]` notation is actually syntactic sugar for a more verbose form that
creates the same lists.  The special `empty` value is equivalent to `[]`, and
the `link` function attaches a value to the front of an existing list:

    check:
      empty is []
      link(1, empty) is [1]
      link(1, link(2, empty)) is [1,2]
      link(empty, link(empty, empty)) is [[],[]]
    end

## Functions

In Pyret, most functions are defined with a function declaration.
A function declaration looks like:

    fun square(n):
      n * n
    end

This binds the name `square` to a function.  Note that Pyret has
no explicit `return` keyword, and the function body “returns”
whatever it evaluates to.  We can call `square` by passing
arguments in parentheses:

    check:
      square(4) is 16
      square(2) is 4
    end

Since there are often tests that go along with a function
declaration, a declaration can directly attach a testing block
using `where:`.  So we could write the above as:

    fun square(n):
      n * n
    where:
      square(4) is 16
      square(2) is 4
    end

This runs the same tests as the `check:` block, but it is now
obvious to the reader (and to the programming environment!) that
these tests go with the `square` function.

Functions are first-class values in Pyret, so they can be passed
as arguments to other functions or returned from them:

    fun apply-twice(f, x):
      f(f(x))
    where:
      apply-twice(square, 2) is 16
      apply-twice(square, 3) is 81
    end

Functions don't need to have names.  An anonymous function can be
written by eliding the `where:` clause and the name:

    check:
      apply-twice(fun(x): x + 1 end, 10) is 12
    end

You can (and should) add documentation to your functions to
describe their purpose. The best way to do that is with `doc:` For
example:

    fun apply-twice(f, x):
      doc: "Applies f to x, then applies f to that result.
            f should be a function that takes a single argument."
      f(f(x))
    end

## Data

Pyret has a builtin form for declaring and manipulating structured data.

### Definitions

One example that you've already seen is `List`. A list is either
`empty` (written `[]` as shorthand) or it is a `link` of an element
and another list. While very important to the code that we write,
`List`s are not a special internal value, they are just defined with
the `data` form. A simplified version of it is:

    data List:
      | empty
      | link(first, rest)
    end

Though this won't actually run, because Pyret will complain that you're trying
to re-define `List`).  This is the general syntax of a `data` definition: the
name of the data type, then a list of one or more variants, which may have
attributes (like `link`) does, or may not. The values of the type are
constructed just like the variants are written:

    x = empty
    y = link(10, empty)

This is the basic form. In addition to the functions to construct the values,
you also get functions to check whether values are of the type. In this case,
there are two functions: `is-empty` checks if a value is the `empty` value, and
`is-link` checks if a value is a `link` value.

Note that `empty` is a singleton value, the only one of its kind.  It was
constructed that way because the data definition didn't declare any fields for
it (as with `link`).

There's more that we can do with `data`.  Methods can be added to `data` by
attaching them to the variants using `with:`:

    data MyList:
      | my-empty with:
        length(self): 0 end
      | my-link(first, rest) with:
        length(self): 1 + self.rest.length() end
    where:
      my-link(1, my-link(2, my-empty)).length() is 2
      my-empty.length() is 0
    end

We see here that each instance of a `my-link` or `my-empty` has a member named
`length` that can be accessed with `.`, and then called like a function.  Also
note that `data`, like `fun`, can have `where:` blocks for defining tests that
go along with the data definition.

### Cases

A common pattern is to do different things based on the variant of your `data`
definition: a program that dispatches over the different cases of data.  The
`cases` expression allows you to write branches just like the data definition.
For example:

    fun length(l):
      cases(List) l
        | empty => 0
        | link(f, r) => 1 + length(r)
      end
    end

Note that if you don't care about a specific attribute, you can always replace
it with an underscore. Since we did not use `f` in the previous example, we
could write it as:

    fun length(l):
      cases(List) l
        | empty => 0
        | link(_, r) => 1 + length(r)
      end
    end

Which makes it clearer to the reader, especially if the blocks become
large, what we are and aren't going to use.

Finally, it is an error, caught at runtime, to pass a value that isn't
of the type inside the `cases`. And, you don't have to provide all the
variants, and you can provide them in whatever order you want. If you
want to have a catch-all, you can use `else`. For example:

    check:
      0 is cases(List) empty:
        | link(first, _) => first
        | else => 0
      end
    end

It is an error to not match any branch.

## Annotations

Pyret is not currently a typed language, but it allows type-like annotations
that are checked when running your programs. In the future, these will be
checked statically, so that your annotated programs will become safer without
paying any runtime cost. Annotations can be added to function arguments, to
variable bindings, and to the attributes in data variants. For base types, the
annotations should look like:

    fun string-identity(y :: String): y end
    data BinTree:
      | leaf
      | node(value :: Number, left :: BinTree, right :: BinTree)
    end

Note that `BinTree`, the name of a datatype, can be used as an annotation.  Any
data type that you define can also be used in an annotation.  These annotations
will stop the program from creating a `BinTree` with fields that don't match
the annotations:

    check:
      string-identity(0) raises "expected String"
      node("not-a-num", leaf, leaf) raises "expected Number"
      node(37, leaf, "not-a-bin-tree") raises "expected BinTree"
    end

You can also define arbitrary predicates. For example:

    fun non-negative(n :: Number) -> Bool:
      n >= 0
    end

    fun replicate(n :: Number(non-negative), e) -> List:
      if n == 0:
        []
      else:
        link(e, replicate(n - 1, e))
      end
    end

And if you were to call `replicate` with a negative number, it would
not run (instead of running forever):

    check:
      replicate(-1, "val") raises "value did not match predicate"
    end



## Control

### For loops

We present the common pattern of iteration in a simplified syntax. To `map`
over a list, running some block of code to produce a new value for each
existing value, we can write:

    x = for map(elem from [1,2,3,4]):
      elem + 2
    end
    check:
      x is [3,4,5,6]
    end

Note a few things:

- `for` is an expression, and is legal to write on the right hand side of a
  binding
- The whole `for` expression evaluates to a value (in this case, a new list)

The `for` syntax is designed to create patterns of functional iteration.
Indeed, there are several other built in functions that work with `for`:

    z = for filter(elem from [1,2,3,4]):
      elem < 3
    end
    check:
      z is [1,2]
    end

    y = for fold(sum from 0, elem from [1,2,3]):
      sum + elem
    end
    check:
      y is 6
    end

And you are free to define your own `for` operators.  A `for` operator is a
function that takes a function as its first argument, and a number of other
values as the rest of its arguments.  The function argument is expected to have
the same arity as the number of initial values.  To use the operator in a `for`
expression, the `for` header should have a number of `from` bindings equal to
this arity.  For example:

    fun keep-every-other(body-fun, l):
      fun iter(flip, lst):
        cases(List) lst:
          | empty => empty
          | link(first, rst) =>
            if flip:
              link(body-fun(first), iter(not flip, rst))
            else:
              iter(not flip, rst)
            end
        end
      end
      iter(true, l)
    end

    w = for keep-every-other(elt from range(0,10)):
      elt + 1
    end
    check:
      w is [1, 3, 5, 7, 9]
    end

What the `for` expression does is create a new function from the names on the
left-hand sides of the `from` clauses and the body, and pass that new function
along with the values on the right of `from` to the operator
(`keep-every-other`, in this case).

### If

Branching on conditionals is an `if` branch followed by zero or more
`else if` branches and an optional `else` branch. It is a runtime
error to not match one of the branches - if you are writing code to purely
cause side effects, write a `when` block instead. A few examples:

    if x < 10:
      print("Hello")
    else if x > 20:
      print("Yay")
    else:
      print("Aww...")
    end

    if false:
      # this is a runtime error, you need an else branch
    end

    if true and false:
      #...
    else:
      #...
    end

Pyret expects that `if` expressions are _total_, and signals an error if
control falls off the end:

    check:
      fun if-falls-off():
        if false:
          ""
        else if false:
          ""
        end
      end
      if-falls-off() raises "if: no tests matched"
    end

For this reason, Pyret syntactically rules out single-branch if expressions,
which make little sense given this rule.

### When blocks

Sometimes there is certain code that should only be run when something is true.
This is code that exists solely to _do_ something. For example:

    when n > 10:
      print("Oh No!")
    end

This covers the cases that single-branch if expressions are usually used for,
but makes it explicit that the body is used for its side effects.

### Blocks

There are many block forms in Pyret. In any block, any number of
statements / expressions can be put. The last one will be what the block
evaluates to, which has different meanings depending on the context. The
blocks are:

- top level of a `.arr` file (or Captain Teach/DrRacket editor)
- `fun` bodies
- `method` bodies
- `check` bodies
- `if`, `else if`, and `else` branches
- inside `when`
- between `try` and `except`, and `except` and `end`
- in the branches of `cases`

