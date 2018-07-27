provide *

import global as _
import resugar-lib as RL
import resugar-visitor as RV
import json as JSON

rules = ```
sugar flat-prim-app:
  | (flat-prim-app) => (prim-app-info-c false)
end

sugar g-id:
  | (g-id x) => (s-id (s-global x))
end

sugar b-id:
  | (b-id x) => (s-dot (g-id "builtins") x)
end

sugar s-method-app:
  | (s-method-app l2 obj field args) => (s-app (s-dot @l2 obj field) args)
end


sugar check-ann:
  | (check-ann expr ann) =>
    (fresh x (s-let-expr [(s-let-bind (s-bind false x ann) expr)] (s-id x) true))
end

sugar mk-s-bind:
  | (mk-s-bind name) => (s-bind false name (a-blank))
end

sugar mk-s-lam:
  | (mk-s-lam params body) =>
    (s-lam "" [] params (a-blank) "" body none none false)
end

sugar not:
  | (not x) => (s-prim-app "not" [x] (flat-prim-app))
end

sugar is-boolean:
  | (is-boolean x) => (s-prim-app "isBoolean" [x] (flat-prim-app))
end


################################################################################
# DONE: s-if, s-if-else, s-if-pipe, s-if-pipe-else
################################################################################

sugar no-branch-exn:
  | (no-branch-exn @l typ) =>
    (s-prim-app "throwNoBranchesMatched" [(s-srcloc l) (s-str typ)] (flat-prim-app))
end

sugar s-if:
  | (s-if branches blocky) => (s-if-else branches (no-branch-exn "if") blocky)
end

sugar s-if-pipe:
  | (s-if-pipe branches blocky) =>
    (s-if-else branches (no-branch-exn "ask") blocky)
end

sugar s-if-pipe-else:
  | (s-if-pipe-else branches else blocky) => (s-if-else branches else blocky)
end

sugar s-if-else:
  | (s-if-else [] else blocky) => else
  | (s-if-else [branch rest_{x} ...x] else blocky) =>
    <s-if-else [branch] (s-if-else [rest_{x} ...x] else blocky) blocky>
end

################################################################################
# DONE: s-template, s-user-block, s-paren
################################################################################

sugar s-template:
  | (s-template @l) =>
    (s-prim-app "throwUnfinishedTemplate" [(s-srcloc l)] (flat-prim-app))
end

sugar s-user-block:
  | (s-user-block body) => body
end

sugar s-paren:
  | (s-paren body) => body
end


################################################################################
# DONE: s-table
################################################################################

sugar s-field-name:
  | (s-field-name @l name ann) => {s-field-name l name ann}
end

sugar s-table-row:
  | (s-table-row elems) => {s-table-row elems}
end

sugar s-table:
  | (s-table [{s-field-name l_{i} name_{i} ann_{i}} ...i] [{s-table-row [val_{i j} ...i]} ...j]) =>
    (s-prim-app "makeTable"
      [(s-array [(s-str @l_{i} name_{i}) ...i])
       (s-array [(s-array [(check-ann val_{i j} ann_{i}) ...i]) ...j])]
      (flat-prim-app))
end
```

resugarer = RL.resugar(rules)

fun resugar(ast):
  resugarer(ast.visit(RV.ast-to-term-visitor).serialize())
    ^ JSON.read-json
    ^ RV.term-to-ast
    ^ _.tosource()
    ^ _.pretty(80)
    ^ _.join-str("\n")
    # ^ print
end
