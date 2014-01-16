#lang pyret

provide *
import "compile-structs.arr" as C
import ast as A
import "anf.arr" as N
import "ast-split.arr" as AS
import "anf-simple-compile.arr" as AC
import file as F

fun pretty(src): src.tosource().pretty(80).join-str("\n") end

fun make-compiled-pyret(program-ast, env):

  anfed = N.anf-program(program-ast)
  split = AS.ast-split(anfed.body)
  compiled = AC.compile(split)
  code = compiled.tosource().pretty(80).join-str("\n")
  c = C.ok(code)
  
  {
    pyret-to-js-standalone: fun():
      runtime = F.file-to-string("runtime-anf.js")
      standalone = runtime + "\n" +
      "var program = \n" +
      code +
      "\nvar rt = PYRET_ANF.makeRuntime({ stdout: function(str) { console.log(str); }});\n" +
      "rt.run(program, rt.namespace, function(result) {
         if(rt.isSuccessResult(result)) {
            console.log(result);
            process.exit(0);
         } else if (rt.isFailureResult(result)) {
            console.error('Pyret terminated with an error');
            console.error(result);
            process.exit(1);
         }
      });"
      C.ok(standalone)
    end,
    pyret-to-js-runnable: fun():
      c
    end
  }
end

