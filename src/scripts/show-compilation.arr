#lang pyret

import cmdline as C
import parse-pyret as P
import "./arr/compiler/desugar.arr" as D
import "./arr/compiler/compile.arr" as CM
import "./arr/compiler/compile-structs.arr" as CS
import "./arr/compiler/resolve-scope.arr" as R
import "./arr/compiler/ast-util.arr" as U
import "./arr/compiler/anf.arr" as N
import "./arr/compiler/ast-split.arr" as AS
import file as F

options = {
  width: C.next-val-default(C.Number, 80, some("w"), C.once, "Pretty-printed width")
}

parsed-options = C.parse-cmdline(options)

cases (C.ParsedArguments) parsed-options:
  | success(opts, rest) =>
    print("Success")
    cases (List) rest:
      | empty => print("Require a file name")
      | link(file, _) =>
        print("File is " + file)
        file-contents = F.file-to-string(file)
        print("")
        print("Read file:")
        print(file-contents)
        parsed = P.surface-parse(file-contents, file)
        print("")
        print("Parsed:")
        each(print, parsed.tosource().pretty(80))

        resolved = R.desugar-scope(U.append-nothing-if-necessary(parsed), CS.minimal-builtins)
        print("")
        print("Resolved:")
        each(print, resolved.tosource().pretty(80))
        
        desugared = R.resolve-names(D.desugar(resolved, CS.minimal-builtins), CS.minimal-builtins)
        print("")
        print("Desugared:")
        each(print, desugared.tosource().pretty(80))

        
        cleaned = desugared.visit(U.merge-nested-blocks)
        .visit(U.flatten-single-blocks)
        .visit(U.link-list-visitor(CS.minimal-builtins))

        anfed = N.anf-program(cleaned)
        print("")
        print("ANFed:")
        each(print, anfed.body.tosource().pretty(80))

        split = AS.ast-split(anfed.body)
        print("")
        print("Split:")
        each(print, split.tosource().pretty(80))
        
        # comp = CM.compile-js(file-contents, file, CS.standard-builtins, {check-mode: false})
        # cases(CM.CompileResult) comp:
        #   | ok(c) =>
        #     print("")
        #     print("Generated JS:")
        #     print(c.pyret-to-js-pretty())
        #   | err(problems) =>
        #     print("")
        #     print("Compilation failed:")
        #     each(print, problems.map(tostring))
        # end
    end
  | arg-error(m, _) =>
    each(print,  ("Error: " + m) ^ link(C.usage-info(options)))
end
print("Finished")
