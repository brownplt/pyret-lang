import error as E
import parse-pyret as P
import render-error-display as RED

# https://github.com/brownplt/pyret-lang/issues/1845
#
# equality-failure's render-fancy-reason assumed the AST at the error
# location was a binary operator (`f == g`) and read `ast.left` / `ast.right`,
# so an equality error raised by a call (`identical(f, g)`) failed while being
# rendered. This module calls render-fancy-reason directly, the way CPO does,
# with the parsed source of each comparison as the error location's AST.

fun f(): 1 end

fun render-fancy(thunk, src):
  e = exn-unwrap(run-task(thunk).v)
  ast = P.surface-parse(src, "identical-fn-render").block.stmts.first
  rendered = e.render-fancy-reason(
    lam(_, _): some(ast.l) end,
    lam(_): true end,
    lam(_): some(ast) end)
  RED.display-to-string(rendered, torepr, empty)
end

check "equality failure on functions renders for calls and operators":
  render-fancy(lam(): identical(f, f) end, "identical(f, f)")
    satisfies string-contains(_, "The values are not comparable.")
  render-fancy(lam(): equal-always(f, f) end, "equal-always(f, f)")
    satisfies string-contains(_, "The values are not comparable.")
  render-fancy(lam(): f == f end, "f == f")
    satisfies string-contains(_, "The values are not comparable.")
end
