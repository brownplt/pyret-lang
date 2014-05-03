#lang pyret

import cmdline as C
import parse-pyret as P
import "compiler/desugar.arr" as D
import "compiler/desugar-check.arr" as DC
import "compiler/anf.arr" as A
import "compiler/compile.arr" as CM
import "compiler/compile-structs.arr" as CS
import "compiler/resolve-scope.arr" as R
import "compiler/ast-util.arr" as U
import "compiler/anf.arr" as N
import "compiler/ast-split.arr" as AS
import "compiler/js-of-pyret.arr" as JS
import "compiler/desugar-check.arr" as CH
import file as F

options = {
  width: C.next-val-default(C.Number, 80, some("w"), C.once, "Pretty-printed width"),
  dialect: C.next-val-default(C.String, "Pyret", some("d"), C.once, "Dialect to use")
}

parsed-options = C.parse-cmdline(options)

cases (C.ParsedArguments) parsed-options:
  | success(opts, rest) =>
    print-width = opts.get("width")
    dialect = opts.get("dialect")
    print("Success")
    cases (List) rest:
      | empty => print("Require a file name")
      | link(file, _) =>
        print("File is " + file)
        file-contents = F.file-to-string(file)
        print("")
        print("Read file:")
        print(file-contents)
        parsed = P.parse-dialect(dialect, file-contents, file)
        print("")
        print("Parsed:")
        each(print, parsed.tosource().pretty(print-width))

        scoped = R.desugar-scope(DC.desugar-check(U.append-nothing-if-necessary(parsed)), CS.minimal-builtins)
        print("")
        print("Scoped:")
        each(print, scoped.tosource().pretty(print-width))
        
        desugared = D.desugar(R.resolve-names(scoped, CS.minimal-builtins).ast, CS.minimal-builtins)
        print("")
        print("Desugared:")
        each(print, desugared.tosource().pretty(print-width))

        cleaned = desugared.visit(U.merge-nested-blocks)
        .visit(U.flatten-single-blocks)
        .visit(U.link-list-visitor(CS.minimal-builtins))

        anfed = N.anf-program(cleaned)
        print("")
        print("ANFed:")
        each(print, anfed.tosource().pretty(print-width))

        unsafecomp = JS.make-unsafe-compiled-pyret(cleaned, CS.standard-builtins)
        print("")
        print("Non-stacksafe generated JS:")
        print(unsafecomp.pyret-to-js-pretty())
        
        split = AS.ast-split(anfed.body)
        print("")
        print("Split:")
        each(print, split.tosource().pretty(print-width))

        comp = CM.compile-js(dialect, file-contents, file, CS.standard-builtins, {check-mode: false})
        cases(CM.CompileResult) comp:
          | ok(c) =>
            print("")
            print("Generated JS:")
            print(c.pyret-to-js-pretty())
          | err(problems) =>
            print("")
            print("Compilation failed:")
            each(print, problems.map(tostring))
        end
    end
  | arg-error(m, _) =>
    each(print,  ("Error: " + m) ^ link(C.usage-info(options)))
end
print("Finished")
