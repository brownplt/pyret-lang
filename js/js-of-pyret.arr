#lang pyret

provide *
import "compile-structs.arr" as C
import ast as A
import "anf.arr" as N
import "ast-split.arr" as AS
import "anf-simple-compile.arr" as AC

fun pyret-to-js(
    program-ast :: A.Program,
    env :: C.CompileEnvironment
  ) -> C.CompileResult:

  # Define A-Normal Form
  # Convert to A-Normal Form
  # Alpha-rename all identifiers
  # Lift variables to the top-level
  # Split up at function calls
  # Convert to JS, with stack frames at each function call

  anfed = N.anf-program(program-ast)
  split = AS.ast-split(anfed.body)
  C.ok(AC.compile(split))

end

fun expr-to-js(program-ast):

end

