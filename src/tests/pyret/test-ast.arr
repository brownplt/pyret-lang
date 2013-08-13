#lang pyret

import ast as A

pred = checkers.check-pred
eq = checkers.check-equals
is_true = checkers.check-true
is_false = checkers.check-true

fun just-parse(str):
  A.parse(str, "test-ast.arr", {["check"]: false}).pre-desugar
where:
  five = just-parse("5")

  pred("Is a program", five, A.is-s_program)
  eq("Has the given label", five.l.file, "test-ast.arr")
  pred("Has no imports", five.imports, list.is-empty)
  eq("Has a number in the block", five.block.stmts.first.n, 5)

  with_imports = just-parse("import foo as bar provide *")
  eq("Length of headers is 2", with_imports.imports.length(), 2)
  pred("Has a provide_all", with_imports.imports.rest.first, A.is-s_provide_all)
  pred("Has an import", with_imports.imports.first, A.is-s_import)
  pred("Has a const import", with_imports.imports.first.file, A.is-s_const_import)

  for_loop = A.parse("for map(a from [1,2,3]): a + 1 end", "for-loop", {['check']: false})
  pred("Is a for loop",
       for_loop.pre-desugar.block.stmts.first,
       A.is-s_for)
  pred("Turns into an application",
       for_loop.post-desugar.block.stmts.first,
       A.is-s_app)

end
