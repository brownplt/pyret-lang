# IO Tests

We test IO with a harness which executes all tests located in the `tests/io-tests/tests/` directory with an `.arr` file extension. The harness accepts either a test input to inject and an expected output to assert or an error message to assert (along with asserting a failure is expected). The two scenarios are outlined below.

### Situation 1: Expected Success

**Input**

```
###< INPUT (string)
```

**Output**

```
###> OUTPUT (regex)
```

**Example:**

The following example inserts `FooBarBaz` and expects _both_ the prompt `Who is this?` and the input `FooBarBaz` to exist in the output.

```
###> .*Who is this?.*FooBarBaz.*
###< FooBarBaz

import iolib as IO

print(IO.prompt("Who is this? "))

```

### Situation 2: Expected Failure

**Error Message**
```
###! ERROR OUTPUT (regex)
```

**Example**:

The following example asserts that prompt call will fail based on the arity check, outputting the given error message.

```
###< foo
###! .*Expected to get 1 argument at file:.* but got these 0 arguments.*

import iolib as IO

print(IO.prompt())

```


