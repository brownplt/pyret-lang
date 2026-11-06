import error as E
import parse-pyret as P

fun f(x): x end
fun g(x): x end

fun render(exn, src):
  ast = P.surface-parse(src, "identical-fn-error").block.stmts.first
  exn.render-fancy-reason(lam(_, _): some(ast.l) end, lam(_): true end, lam(_): some(ast) end)
end

check:
  exn = exn-unwrap(run-task(lam(): identical(f, g) end).v)
  exn satisfies E.is-equality-failure
  render(exn, "identical(f, g)") does-not-raise
  render(exn, "f <=> g") does-not-raise
end
