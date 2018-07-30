All tests run through jest (https://facebook.github.io/jest/)

The entrypoints are all named `*.test.js` in this directory (`tests/`).

Types of tests:

## `simple-output`

This directory contains single .arr files whose first line must look like:

```
### <expected string to find on stdout here>
```

The test runner compiles and runs all of these files and checks that the output
provided is a substring of the output from running the program.

The compiled files go to `tests/compiled`, which is cleared before running each
individual test.

