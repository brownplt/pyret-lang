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
`check:` block and a testing assertion.  Here's a simple where
block that uses the `is` assertion:

    check:
      "Ahoy " + "world!" is "Ahoy world!"
    end

Running this will give you:

    Looks shipshape, your 1 test passed, mate!

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

The usual flow of writing a Pyret program involves writing tests
and your code, running your code to check the test output, and
repeating.  The more tests you write, the more useful feedback you
get.

The examples in this tour will all be presented in testing blocks
(you'll see one kind other than `check:` later).  Unless we're
explicitly pointing out a failure, you can assume that the tests
all pass, and we're trying to show correct behavior.


## Primitive Values and Operators

Primitives values are the basic building blocks of the language;
structured data exists to organize computation over a small set of
primitive values.  We describe Pyret's primitives here.

### Numbers

Numbers can be written with or without decimals.  For example:

    check:
      5.0 is 5
    end

Pyret defines a number of binary operators over numbers (the full
list is available at [FILL]):

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

This runs the same tests as the `check:` block, but not it is
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


### Control

#### For loops

We present the common pattern of iteration in a simplified syntax. To `map` over a list,
running some block of code to produce a new value for each existing value, we can write:

    x = for map(elem from [1,2,3,4]):
      elem + 2
    end
    check:
     x is [3,4,5,6]
    end

There are also several other built in functions for this purpose:

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

And you are free to define your own `for` operators - they are functions that take
as their first arguments a function with the argument names from the left side of
the `from` clauses and has the body of the `for` block, and then the rest of
the arguments are the values from the right side of the `from` clauses. For example:

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

#### If

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

#### When blocks

Sometimes there is certain code that should only be run when something is true. This is
code that exists solely to _do_ something. For example:

    when n > 10:
      print("Oh No!")
    end


#### Exceptions

Any value can be `raise`d, which causes control to immediately transfer to the
`catch` block of the closest (in terms of nesting) `try/except` block. The value
raised will be bound to the identifier inside the `except` clause. For example:

    try:
      10
      raise("Help")
      20 # control never reaches here
    except(e):
      e # is "Help"
    end

There can be any number of `try/except` blocks nested - the exception will
hit the first one available. For example:


    try:
      try:
        10
        raise("Help")
        20 # control never reaches here
      except(e):
        e # is "Help"
      end
    except(e):
      # control never reaches here
    end


#### Blocks

There are many block forms in Pyret. In any block, any number of
statements / expressions can be put. The last one will be what the block
evaluates to, which has different meanings depending on the context. The
blocks are:

- top level of a `.arr` file (or Captain Teach editor)
- `function` bodies
- `method` bodies
- `check` bodies
- `if`, `else if`, and `else` branches
- inside `when`
- between `try` and `except`, and `except` and `end`
- in the branches of `cases`

### Data

Pyret supports variant data types. This means that a single type may have
several examples of it, with different constructors.

#### Definitions

One example that you've already seen is `List`. A list is either
`empty` (written `[]` as shorthand) or it is a `link` of an element
and another list. While very important to the code that we write,
`List`s are not a special internal value, they are just defined with
the `data` form. A simplified version of it is:

    data List:
      | empty
      | link(first, rest)
    end

This is the general syntax of a `data` definition: the name of the data type,
then a list of one or more variants, which may have attributes (like `link`)
does, or may not. The values of the type are constructed just like the variants
are written:

    x = empty
    y = link(10, empty)

This is the basic form. In addition to the functions to construct the
values, you also get functions to check whether values are of the
type. In this case, there are three functions: `List` checks if a
value is any type of `List`, `is-empty` checks if a value is the
`empty` value, and `is-link` checks if a value is a `link` value.

There's more that we can do with `data`. For example, if you define an
`_equals` method on your data type, it can be compared with
`==`. Methods can be added to `data` in two ways. They can either be
attached to all values of the data type, or just some variants. If you
define them per-variant, you should be careful if you don't define the
same methods on all variants, because using them might be hard! Here are
examples of the two ways:

    data List:
      | empty with:
        length(self): 0 end
      | link(first, rest) with:
        length(self): 1 + self.rest.length() end
    sharing:
      my-special-method(self):
        print("I'm a list!")
      end
    end

In both forms, there can be any number of comma-separated methods.

#### Cases

A common pattern is to do different things based on the variant of
your `data` definition. You could use `if` statements, but it gets
clumsy quickly. Instead, `cases` allows you to write branches just like the
data definition. For example:

    cases(List) x:
      | empty => print("An empty list!")
      | link(first, rest) => print("A non-empty list!")
    end

Note that if you don't care about a specific attribute, you can always
replace it with an underscore. Since we use neither `first` nor `rest`
in the previous example, we could write it as:

    cases(List) x:
      | empty => print("An empty list!")
      | link(_, _) => print("A non-empty list!")
    end

Which makes it clearer to the reader, especially if the blocks become
large, what we are and aren't going to use.

Finally, it is an error, caught at runtime, to pass a value that isn't
of the type inside the `cases`. And, you don't have to provide all the
variants, and you can provide them in whatever order you want. If you
want to have a catch-all, `else` is valid. For example:

    cases(List) x:
      | link(first, _) => first
      | else => nothing
    end

It is an error to not match any branch, so if you don't include all
your variants, either include an `else` or be sure that only the
variants listed will ever be passed in.


### Annotations

Pyret is not currently a typed language, but it allows type-like
annotations that are checked when running your programs. In the
future, some or most of these will be turned into compile time
type-checking, so that your annotated programs will become safer
without paying any runtime cost. Annotations can be added to function
arguments, to variable bindings, and to the attributes in data
variants. For base types, the annotations should look like:

    x :: Number = 10
    fun(y :: String) -> String: x end # an error
    data Foo:
      | foo(a :: Bool, b :: Nothing, c :: List, d :: Any)
    end

`Any` is a special annotation that matches anything. You can leave it
off, but it makes it clear to anyone reading your code (including
yourself) that you really mean it to be anything.

If you noticed `List` in there, you'll realize that any data type
that you define can also be used in an annotation.

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
not run (instead of running forever).


