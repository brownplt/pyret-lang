## Pyret Language Reference

### Basics

#### Hello world

    print("Hello World!")

#### Comments

Are single line, starting from the `#` character and running to the end of the line.

#### Whitespace

Is not significant except insofar as to separate pieces of syntax. So
there are places where you must place _some_ whitespace, but
indentation, nor how much whitespace (and what combination of newlines
and spaces) never matters.


#### Separators

There are also no line end separators. Pyret is semicolonless. 

#### Expressions

Most syntax in the language are expression forms, which means they can
appear almost anywhere in the syntax where it would make sense. The
exception are the binding forms - named function definitions, data
declarations, and variable binding. Binding forms can only appear at
the block level (ie, at the top level and within functions / methods /
where blocks).

#### Objects

Every value is an object. This means that the basic object operations
(adding fields and methods, looking stuff up, `branding` (to be
defined later) can be done on any value.)

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

    {foo: 10}.foo # evaluates to 10

    {["foobar"]: 300}.foobar # evaluates to 300

#### Lists

Written as square bracket comma-separated values, lists can have any
value in them (they are heterogeneous). Lists are objects, so the
first element in the list (sometimes known as the head in other
languages) is accessed as the `first` field, and the rest of the list
(which is another list, potentially empty) is the `rest` field.

    [1,2,3].first # evaluates to 1
    [1,2,3].rest # evaluates to [2,3]
    [].first # is an error, since the list is empty

List notation is actually syntax sugar for a data definition in the Pyret
standard libraries. `[1,2,3]` is really `link(1,link(2,link(3,empty)))`
where `empty` is one constructor, that is an empty list, and `link` is another,
that takes a value and another list.

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

### Variables and Named Functions

We can bind any value to a name with `=`. This value cannot be changed
(though the binding can be shadowed). For example:

    f = fun(x): x end
    f(10) # evaluates to 10

Since we often want names for our functions, the following shorthand is possible:

    fun f(x): x end

Which will do the same as the previous example.

Sometimes you want to be able to change the value in a variable. For
example, we might have a deeply nested computation that needs to record that
sometimes specific happened, and threading the result out isn't convenient.
For this, we have a separate declaration, and a way to update those variables:

    var x = 10
    # ...
    x := 20

### More Values

#### Methods

Methods are a separate kind of value from functions. Given a function,
you can get a corresponding method, and the same is true in reverse,
but methods behave differently when inside objects. A methods is
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

We use infix `and` and `or`. These work on booleans, and can also work on your
own datatypes. You must define `_and` and/or `_or` methods, and note that they
pass their second argument as a zero-argument function. This is so that they
can be short-circuiting - if the left side of an `and` is false, there is no
need to evaluate the right side (so in your definitions, you can elect to
call the function or not). For example:

    true and true and false # evaluates to false
    true and (false or true) # evaluates to true
    
    
### Control

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

#### For loops

We present the common pattern of iteration in a simplified syntax. To `map` over a list,
running some block of code to produce a new value for each existing value, we can write:

    x = for map(elem from [1,2,3,4]):
      elem + 2
    end
    # x is [3,4,5,6]

There are also several other built in functions for this purpose:

    x = for filter(elem from [1,2,3,4])
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
the arguments are the values from the right side of the `from` clauses.


#### If

#### Case

#### When blocks

Sometimes there is certain code that should only be run when something is true. This is
code that exists solely to _do_ something. For example:

    when n > 10:
      print("Oh No!")
    end

### Data Definitions