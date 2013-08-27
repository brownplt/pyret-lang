## Pyret Language Introduction

### Basics

#### Hello world

    print("Hello World!")

Bonus: when you call `print()`, it first calls the builtin
`tostring()` function on the argument. This shows sensible representations
for base values, but for more complicated objects, if you define
a `tostring` method, it will use that instead.

#### Comments

Are single line, starting from the `#` character and running to the end of the line.

    # This is a comment
    foo(bar) # this is another comment

#### Whitespace

Is not significant except insofar as to separate pieces of syntax. So
there are places where you must place _some_ whitespace, but
indentation, nor how much whitespace (and what combination of newlines
and spaces) ever matters.

On the other hand, whitespace is sometimes not allowed: if a keyword is followed
by a colon, no whitespace may be present between the two. Also, `else if` must be
written exactly this way - exactly one space between `else` and `if`.


#### Separators

There are also no line end separators. Pyret is semicolonless.

#### Expressions

Most syntax in the language are expression forms, which means they can
appear almost anywhere in the syntax. The
exception are:

1. Binding forms - named function definitions, data
   declarations, and identifier/variable binding.
2. Variable assignment `x := v`

These forms can only appear in [blocks](#blocks).

#### Objects

Most values in Pyret are objects. This means that the basic object operations
(adding fields and methods, looking stuff up, `branding` (to be
defined later) can be done on any of the normal types (the exceptions to this
rule are the special `nothing` value, and values you get when interacting with
the FFI, which gets you special wrapped up racket or javascript values).

### Values

#### Numbers

Written with or without decimals. Examples:

    4
    5.12
    -79

#### Booleans

There are only two:

    true
    false

#### Strings

Written as double quoted unicode strings. They may span multiple
lines. Character escapes like \n work, and the double quote character
can be included if escaped like \".

    "hello world"

    "hello out
     there"

    "an example\n\nwith explicit newline\ncharacters"

#### Nothing

`nothing` is a value that doesn't do anything. You can return it,
check for it, and in some cases, like if you have an empty function
body, it will be returned to you. You can't add fields to it. You write it like:

    nothing

#### Objects

Written as dictionary literals, in curly brace syntax. Keys are always
strings, and can be computed from an arbitrary expression, and should
be enclosed in square brackets. In the case of identifier-like keys
(ie, no spaces or other characters illegal in identifiers), they can
be written without the quotes or square brackets.

    {["foo" + "bar!"]: 10, baz: 20}

The fields can be accessed with dotted notation, with the same rules
about square brackets for arbitrary expressions, and identifier-like keys
written without.

    {foo: 10}.["fo" + "o"] # evaluates to 10

    {["foobar"]: 300}.foobar # evaluates to 300

Objects in Pyret are not mutable. To construct a new object with a new or
changed field from an old object, you use the following notation:

    o.{new-field: 10}

You can also add/change many fields:

    o.{fieldA: 10, fieldB: 20}

Or do this multiple times:

    o.{fieldA: 10}.{fieldB: 20}


#### Lists

Written as square bracket comma-separated values, lists can have
values of different types in them (they are heterogeneous). Lists are
objects, so the first element in the list (sometimes known as the head
in other languages) is accessed as the `first` field, and the rest of
the list (which is another list, potentially empty) is the `rest`
field.

    [1,2,3].first # evaluates to 1
    [1,2,3].rest # evaluates to [2,3]
    [].first # is an error, since the list is empty

List notation is actually syntax sugar for a data definition in the Pyret
standard libraries. `[1,2,3]` is really `link(1,link(2,link(3,empty)))`
where `empty` is one constructor, that is an empty list, and `link` is another,
that takes a value and another list. Note that `link` is what you might know as
`cons` from another language.

#### Functions

Function values are written with the `fun` keyword and then a comma separated
list of arguments followed by a body and then the keyword `end`. There is
no `return` statement as found in some languages - the last value produced (ie,
the last statement in the block) is the value that the function returns. For example:

    fun(x,y): x + y end

    fun(y):
      y * y
    end

Functions can be applied with parenthesis and then a comma separated
list of values.

    (fun(x): x end)(10) # evaluates to 10

Note that parenthesis can be added around any expression to either
disambiguate (in the case of binary operators), or simply for
clarity. We could just have easily written the previous example as:

    fun(x): x end(10)

Or:

    fun(x):
      x
    end(10)

But it might be more confusing. In general though, functions will
usually be bound to variables or passed as arguments instead of being used
immediately after construction.

You can (and should) add documentation to your functions. The best way to do that is with `doc`. On the first line of the function, you can write a documentation string. This will be available as the `_doc` attribute on your functions. Also, if you print out a function (or call the `tostring` built-in on it), it will print with the header and the doc string. For example:

    fun foo(x):
      doc: "my great foo function!"
      y = 10
      x + y # NOTE: improve!
    end

    print(foo) # prints "fun foo(x): 'my great foo function' end"

### Variables, Named Functions and Methods

We can bind any value to a name with `=`. This value cannot be changed
(though the binding can be shadowed). For example:

    f = fun(x): x end
    f(10) # evaluates to 10

Since we often want names for our functions, the following shorthand is possible:

    fun f(x): x end

Which will do the same as the previous example. It's also usually
preferred, as thinking / talking about functions is easier when they
have names.

Bindings are visible at the same scope level and in any nested scope
(ie, local function definitions, etc).

Sometimes you want to be able to change the value in a variable. For
example, we might have a deeply nested computation that needs to
record that something specific happened, and threading the result out
isn't convenient.  For this, we have a separate declaration, and a way
to update those variables:

    var x = 10
    # ...
    x := 20

Note that you cannot define a `var` to a identifier that is already
bound. Also, if you try to use an identifier that has not been bound,
that error will be caught before your program is run (or, really, as
soon as you try to run your program, but before we spend any time
executing your code).

#### Methods

Methods are a separate kind of value from functions. Given a function,
you can get a corresponding method, and the same is true in reverse,
but methods behave differently when inside objects. A method is
written with the `method` keyword instead of `fun`. An example:

    method(self): 10 end

When a method is put in an object, it can be called as follows:

    o = {foo: method(self): 10 end}
    o.foo()

The first argument to the method will be bound to the value of the
object when it is called - the rest will be what is passed to the
call. This can be named anything, but we generally follow the
convention of naming it `self`. Since defining methods in objects is a
common pattern, we provide shorthand for the previous example:

    o = {foo(self): 10 end}
    o.foo()

Given a method, you can get a normal function with the `_fun` method, as follows:

    m = method(self): 10 end
    m._fun()

This could then be applied as a normal function, passing in a value for `self`:

    m = method(self): 10 end
    f = m._fun()
    f({bar: 20}) # evaluates to 10

Methods can also have doc strings, like functions, and they also print them out when
you call `tostring()` on them. For example:

    o = {foo(self): doc: "This function needs help..." 10 end}
    tostring(o.foo) # evaluates to "method foo(self): 'This function needs help...' end"

Finally, if you want to just get out the raw method value (this also
will get out any other raw value, but as of now, methods are the only
things that are treated specially), you can access fields with
colon. For example:

    o = { foo(self): 10 end, b: 20 }
    o:b == o.b # is true - there isn't anything special about non-methods
    m = o:foo
    m() # an error - can't apply bare methods
    f = m._fun()
    f({}) # evaluates to 10 - note that you have to apply it to a self
    o2 = o.{ newmeth = m }
    o2.newmeth() # evaluates to 10 - because now we accessed it with normal dot.


### Operators

Pyret does not have precedence for operators. The result is not
defined if you mix multiple types of operators - you must use
parethesis to disambiguate. For example:

    1 + 2 * 3 # undefined, and an error to write
    1 + (2 * 3) # what you should write

#### Equality

Equality works on the built-in values and on objects structurally (ie,
same keys and values). Functions and methods are never equal to
anything. To create objects that have methods or functions within them but can
be compared for equality, any object that has a "_equals" method that takes
`self` and another object and returns `true` or `false` can also be compared.

    2 == 3 # false
    2 <> 4 # true - this is our not equals


#### Arithmetic

The following operators are supported, with their normal math definition on
built in numbers:

    +
    -
    *
    /
    <=
    >=
    >
    <

Furthermore, like with equality, we support using math operators on your own
data types - any object that defines a "_plus" can be used with `+`, similar
for `_minus`, `_divide`, `_times`, `_lessequal`, `_greaterequal`, `_greaterthan`,
`_lessthan`.

#### Boolean

We use infix `and` and `or`, and prefix `not`. These work on booleans,
and can also work on your own datatypes. You must define `_and`, `_or`
and/or `_not` methods, and note that `and`/`or` pass their second argument
as a zero-argument function. This is so that they can be
short-circuiting - if the left side of an `and` is false, there is no
need to evaluate the right side (so in your definitions, you can elect
to call the function or not). For example:

    true and true and false # evaluates to false
    true and (false or true) # evaluates to true
    not (true and false) # evaluates to true

### Control

#### For loops

We present the common pattern of iteration in a simplified syntax. To `map` over a list,
running some block of code to produce a new value for each existing value, we can write:

    x = for map(elem from [1,2,3,4]):
      elem + 2
    end
    # x is [3,4,5,6]

There are also several other built in functions for this purpose:

    x = for filter(elem from [1,2,3,4]):
      elem < 3
    end
    # x is [1,2]

    y = for fold(sum from 0, elem from [1,2,3]):
      sum + elem
    end
    # y is 6

And you are free to define your own `for` operators - they are functions that take
as their first arguments a function with the argument names from the left side of
the `from` clauses and has the body of the `for` block, and then the rest of
the arguments are the values from the right side of the `from` clauses. For example:

    fun every-other(body-fun, lst):
      fun every-other-internal(flip, lst):
        case(List) lst:
          | empty => empty
          | link(first, rst) =>
            link(if flip: body-fun(first) else: first end,
                 iter-alternating-internal(not flip, rst))
        end
      end
      iter-every-other-internal(true, lst)
    end

    for every-other(elt from range(0,10)):
      print(elt)
    end
    # prints 0, 2, 4, 6, 8, 10

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
      # ...
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
      raise "Help"
      20 # control never reaches here
    except(e):
      e # is "Help"
    end

There can be any number of `try/except` blocks nested - the exception will
hit the first one available. For example:


    try:
      try:
        10
        raise "Help"
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


### Check/where blocks

One of the more interesting features of Pyret are it's `check` and
`where` blocks. At the end of any function or data definition, or in
any block, you can add a `where` block, which contains code that
asserts various properties about the function or data definition (or
just tests things in general). You can put a `check` block anywhere,
not attached to a particular function or data definition. If you run
Pyret in `check` mode, we run these blocks. The novel feature is that
you can put `where` blocks on nested functions, and they will be run
with the arguments to the outer function from outer `where` blocks,
which allows sensible testing of nested functions. For example:

    fun fact(n):
      fun fact_(n, acc):
        if n <= 1:
          acc
        else:
          fact_(n - 1, acc * n)
        end
      where:
        fact_(0, 0) is 0
        fact_(n, 0) is 0
        fact_(3, 3) is 18
        fact_(5, 1) is 120
      end
      fact_(n, 1)
    where:
      fact(1) is 1
      fact(5) is 120
      fact(3) is 6
    end

Note that in the inner tests, we were able to use `n`. In this case,
it wasn't very important, but sometimes helper functions only make
sense in the context of outer data, and setting up testing harnesses
can be really hard. In this case, the inner data is provided by the
outer tests (so the inner check block runs 3 times, once each with `n`
defined as 1, 5, and 3).

`check` blocks can go anywhere, like:

    check:
      1 + 1 is 2
    end

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
        link(e, replicate(n - 1))
      end
    end

And if you were to call `replicate` with a negative number, it would
not run (instead of running forever).


### Brands

Data is actually built from lower level constructs within Pyret. What
Pyret uses to verify that different values are indeed different data
types are `brands`.  A brand can be applied to any value (except
`nothing`), and the presence of that brand can be checked later. New
brands can be constructed at any time. For example, if you had an expensive
computation you had to run to verify that a piece of data behaved a certain
way and didn't want to have to re-run it, you could write code like:

    verified = brander()
    fun expensive-check(x):
      #...
      verified.brand(x)
    end

    fun run(x):
      if not verified.test(x):
        foo(expensive-check(x))
      else:
        foo(x)
      end
    end

    y = []
    z = expensive-check([])

    run(y) # runs expensive-check
    run(z) # doesn't run expensive-check

Note that since objects are not mutable, the `.brand` method returns a
new object with the brand added. You are welcome to use `brander`s for
whatever you want - we think they are an interesting pattern for
controlling a certain kind of truth within a program (of which a type
is just one example).
