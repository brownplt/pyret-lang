#lang pyret

provide *
provide-types *
import file("compile-structs.arr") as C
import file("concat-lists.arr") as CL
import ast as A
import string-dict as SD
import file as F
import file("anf.arr") as N
import file("ast-anf.arr") as AA
import file("anf-loop-compiler.arr") as AL
import file("desugar-check.arr") as CH
import file("desugar.arr") as D
import file("ast-util.arr") as AU
import file("js-ast.arr") as J

# TODO(joe): add methods for printing to module vs static information
data CompiledCodePrinter:
  | ccp-dict(dict :: SD.StringDict) with:
    method to-j-expr(self, d):
      J.j-parens(J.j-obj(for CL.map_list(k from d.keys-list()):
          J.j-field(k, d.get-value(k))
        end))
    end,
    method pyret-to-js-static(self) -> String:
      self.to-j-expr(self.dict.remove("theModule")).to-ugly-source()
    end,
    method print-js-static(self, printer):
      self.to-j-expr(self.dict.remove("theModule")).print-ugly-source(printer)
    end,
    method pyret-to-js-pretty(self, width) -> String:
      self.to-j-expr(self.dict).tosource().pretty(width).join-str("\n")
    end,
    method pyret-to-js-runnable(self) -> String:
      self.to-j-expr(self.dict).to-ugly-source()
    end,
    method print-js-runnable(self, printer):
      self.to-j-expr(self.dict).print-ugly-source(printer)
    end
  | ccp(compiled :: J.JExpr) with:
    method pyret-to-js-pretty(self, width) -> String:
      self.compiled.tosource().pretty(width).join-str("\n")
    end,
    method pyret-to-js-runnable(self) -> String:
      self.compiled.to-ugly-source()
    end,
    method print-js-runnable(self, printer):
      self.compiled.print-ugly-source(printer)
    end
  | ccp-string(compiled :: String) with:
    method pyret-to-js-pretty(self, width) -> String:
      raise("Cannot generate pretty JS from code string")
    end,
    method pyret-to-js-runnable(self) -> String:
      self.compiled
    end,
    method print-js-runnable(self, printer):
      printer(self.compiled)
    end
  | ccp-file(path :: String) with:
    method pyret-to-js-pretty(self, width) -> String:
      raise("Cannot generate pretty JS from code string")
    end,
    method pyret-to-js-runnable(self) -> String block:
      F.file-to-string(self.path)
    end,
    method print-js-runnable(self, printer):
      printer(self.pyret-to-js-runnable())
    end
end

fun compute-max-depth(aexpr :: AA.AExpr,  flatness-env :: SD.StringDict<Option<Number>>) -> Option<Number>:
  # Maybe walk through expr looking for function applications. Look up the depth of those applications
  # in the flatness env and return the max we find + 1 (or 0)
  # ?
  none
end

# Maybe compress Option<Number> into a type like FlatnessInfo or something (maybe something "Info" in the name)
fun make-expr-flatness-env(aexpr :: AA.AExpr, sd :: SD.StringDict<Option<Number>>) -> SD.StringDict<Option<Number>>:
  cases(AA.AExpr) aexpr:
    | a-type-let(_, bind, body) =>
      sd
    | a-let(_, bind, val, body) =>
      new-sd = make-expr-flatness-env(body, sd)
      # If this binds a lambda, then we want to "add" to sd the binding's name -> the lambda's depth
      # I think this should happen after we get the new-sd since the lambda might itself define other
      # lambdas

      new-sd
    | a-arr-let(_, bind, idx, e, body) =>
      # For now do nothing
      sd
    | a-var(_, bind, val, body) =>
      new-sd = make-expr-flatness-env(body, sd)
      new-sd
    | a-seq(_, lettable, expr) =>
      new-sd = make-lettable-flatness-env(lettable, sd)
      make-expr-flatness-env(expr, new-sd)
    | a-lettable(_, l) =>
      make-lettable-flatness-env(l)
  end
end

fun make-lettable-flatness-env(lettable :: AA.ALettable, sd :: SD.StringDict<Option<Number>>) -> SD.StringDict<Option<Number>>:
  cases(AA.ALettable) lettable:
    | a-module(_, answer, dv, dt, provides, types, checks) =>
      # Do nothing for now
      sd
    | a-if(_, c, t, e) =>
      # For now, do nothing
      sd
    | a-assign(_, id, value) =>
      # Should be able to do nothing since a function can't be a value
      sd
    | a-app(_, f, args) =>
      # Do nothing here since args should be values
      # (though we'll have to look at this in our get-flatness function)
      sd
    | a-method-app(_, obj, meth, args) =>
      # same^
      sd
    | a-prim-app(_, f, args) =>
      # Do nothing for now (not even sure what this variant is, maybe calling a builtin?)
      sd

      # Not worrying about these cases yet, though if they all deal with values, should be trivial
    | a-ref(_, ann) => sd
    | a-tuple(_, fields) => sd
    | a-tuple-get(_, tup, index) => sd
    | a-obj(_, fields) => sd
    | a-update(_, supe, fields) => sd
    | a-extend(_, supe, fields) => sd
    | a-dot(_, obj, field) => sd
    | a-colon(_, obj, field) => sd
    | a-get-bang(_, obj, field) =>
      sd
    | a-lam(_, name, args, ret, body) =>
      make-expr-flatness-env(body, sd)
    | a-method(_, name, args, ret, body) =>
      make-expr-flatness-env(body, sd)
    | a-id-var(_, id) =>
      sd
    | a-id-letrec(_, id, safe) =>
      sd
    | a-val(_, v) =>
      sd
  end
end

fun make-prog-flatness-env(anfed :: AA.AProg) -> SD.StringDict<Number> block:
  print(anfed)
  sd = SD.make-string-dict()
  cases(AA.AProg) anfed:
    | a-program(_, prov, imports, body) =>
      make-expr-flatness-env(body, sd)
  end
end

fun make-compiled-pyret(program-ast, env, provides, options) -> CompiledCodePrinter:
  anfed = N.anf-program(program-ast)
  flatness-env = make-prog-flatness-env(anfed)
  compiled = anfed.visit(AL.splitting-compiler(env, provides, options))
  ccp-dict(compiled)
end

fun trace-make-compiled-pyret(trace, phase, program-ast, env, provides, options) block:
  var ret = trace
  anfed = N.anf-program(program-ast)
  ret := phase("ANFed", anfed, ret)
  phase("Generated JS", ccp-dict(anfed.visit(AL.splitting-compiler(env, provides, options))), ret)
end
