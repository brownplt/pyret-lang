#lang pyret

provide *
import "compile-structs.arr" as C

fun pyret-to-js(
    program-ast :: A.Program,
    env :: CompileEnvironment
  ) -> C.CompileResult:

  # Define A-Normal Form
  # Convert to A-Normal Form
  # Alpha-rename all identifiers
  # Lift variables to the top-level
  # Split up at function calls
  # Convert to JS, with stack frames at each function call

end

fun expr-to-js(program-ast):

end

