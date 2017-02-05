# Pyret Lambda Calculus Parser

This library is a simple implementation of a lambda calculus parser,
but it is primarily meant to provide a stubbed version of Pyret's real `trove/ast.arr` file to
test on compiler development (such as register allocation)

## Usage
To build the project, make sure that you have created the 
`pyret-lang` symbolic link and run `make PHASE`, where `PHASE` is
the Pyret compiler phase you would like to use to compile the
project.

For logistical reasons, this repository does not support Windows
(Cygwin/MSYS2 may work, but they are untested).

See `LICENSE` for license information.
