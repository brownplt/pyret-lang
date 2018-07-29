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
    (fresh x (s-let-expr [(s-let-bind (s-bind false (s-name x) ann) expr)]
               (s-id (s-name x)) true))
end

sugar mk-s-bind:
  | (mk-s-bind name) => (s-bind false name (a-blank))
end

sugar mk-s-lam:
  | (mk-s-lam params body) =>
    (s-lam "" [] params (a-blank) "" body none none false)
end

sugar transform-underscore:
  | (transform-underscore <s-underscore>) => (fresh name (s-name name))
  | (transform-underscore non-underscore) => non-underscore
end

################################################################################
# DONE: s-when
################################################################################

sugar s-when:
  | (s-when @l test body blocky) =>
    (fresh cond
      (s-let-expr
        [(s-let-bind (mk-s-bind (s-name cond)) test)]
        (s-if-else [(s-if-branch (s-id (s-name cond)) (s-block [body (g-id "nothing")]))]
                   (g-id "nothing")
                   blocky)
        false))
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

sugar s-if-pipe-branch:
  | (s-if-pipe-branch test body) => (s-if-branch test body)
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
# DONE: s-construct
################################################################################

sugar s-construct-normal:
  | (s-construct-normal) => {s-construct-normal}
end

sugar s-construct-lazy:
  | (s-construct-lazy) => {s-construct-lazy}
end

sugar s-construct-help:
  | (s-construct-help @l constructor js-name id-name elts) =>
    (s-app @(meta get-loc constructor)
           (s-prim-app @(meta get-loc constructor)
                       js-name [constructor (s-str id-name) (s-srcloc l)
                                (s-srcloc (meta get-loc constructor))]
                       (flat-prim-app))
           elts)
end

sugar s-construct:
  | (s-construct {s-construct-normal} constructor []) =>
    (s-construct-help constructor "getMaker0" "make0" [])
  | (s-construct {s-construct-normal} constructor [e1]) =>
    (s-construct-help constructor "getMaker1" "make1" [e1])
  | (s-construct {s-construct-normal} constructor [e1 e2]) =>
    (s-construct-help constructor "getMaker2" "make2" [e1 e2])
  | (s-construct {s-construct-normal} constructor [e1 e2 e3]) =>
    (s-construct-help constructor "getMaker3" "make3" [e1 e2 e3])
  | (s-construct {s-construct-normal} constructor [e1 e2 e3 e4]) =>
    (s-construct-help constructor "getMaker4" "make4" [e1 e2 e3 e4])
  | (s-construct {s-construct-normal} constructor [e1 e2 e3 e4 e5]) =>
    (s-construct-help constructor "getMaker5" "make5" [e1 e2 e3 e4 e5])
  | (s-construct {s-construct-normal} constructor elts) =>
    (s-construct-help constructor "getMaker" "make" [(s-array elts)])
  | (s-construct {s-construct-lazy} constructor [elt_{x} ...x]) =>
    (s-construct-help constructor "getLazyMaker" "lazy-make"
      [(s-array [(mk-s-lam [] elt_{x}) ...x])])
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
# DONE: s-for
################################################################################

sugar s-for-bind:
  | (s-for-bind bind value) => {s-for-bind bind value}
end

sugar s-for:
  | (s-for @l iter [{s-for-bind bind_{i} value_{i}} ...i] ann body blocky) =>
    (s-app
      iter
      [(s-lam
         (meta string-append "<for-body"
                             (meta string-append (meta loc-to-string l) ">"))
         [] [bind_{i} ...i] ann "" body none none blocky)
       value_{i} ...i])
end

################################################################################
# DONE: s-table-sort
################################################################################

sugar s-column-sort:
  | (s-column-sort @l column direction) => {s-column-sort l column direction}
end

sugar ASCENDING:
  | (ASCENDING) => {ASCENDING}
end

sugar DESCENDING:
  | (DESCENDING) => {DESCENDING}
end

sugar s-table-order:
  | (s-table-order table [{s-column-sort l_{i} column_{i} direction_{i}} ...i]) =>
    (s-app (s-dot table "multi-order")
           [(s-array [(table-order-map @l_{i} column_{i} direction_{i}) ...i])])
end

sugar table-order-map:
  | (table-order-map <s-name column> {ASCENDING}) => (s-array [(s-bool true) (s-str (biject name-to-str column))])
  | (table-order-map <s-name column> {DESCENDING}) => (s-array [(s-bool false) (s-str (biject name-to-str column))])
end

###############################################################################

sugar curry-args-acc:
  | (curry-args-acc [] [param_{x} ...x] args) =>
    {pair (biject reverse [(mk-s-bind param_{x}) ...x]) (biject reverse args)}
  | (curry-args-acc [<s-id <s-underscore>> rest_{x} ...x]
                    [param_{y} ...y]
                    [arg_{z} ...z]) =>
    (fresh f (curry-args-acc [rest_{x} ...x] [(s-name f) param_{y} ...y] [(s-id (s-name f)) arg_{z} ...z]))
  | (curry-args-acc [e rest_{x} ...x] [param_{y} ...y] [arg_{z} ...z]) =>
    (curry-args-acc [rest_{x} ...x] [param_{y} ...y] [e arg_{z} ...z])
end


sugar curry-args:
  | (curry-args lst) => (curry-args-acc lst [] [])
end

###############################################################################

sugar curry-binop:
  | (curry-binop args op) => (curry-binop-help (curry-args args) op)
end

sugar curry-binop-help:
  | (curry-binop-help {pair [] args} "op+") => (s-app (s-id (s-global "_plus")) args)
  | (curry-binop-help {pair [] args} "op-") => (s-app (s-id (s-global "_minus")) args)
  | (curry-binop-help {pair [] args} "op*") => (s-app (s-id (s-global "_times")) args)
  | (curry-binop-help {pair [] args} "op/") => (s-app (s-id (s-global "_divide")) args)
  | (curry-binop-help {pair [] args} "op<") => (s-app (s-id (s-global "_lessthan")) args)
  | (curry-binop-help {pair [] args} "op>") => (s-app (s-id (s-global "_greaterthan")) args)
  | (curry-binop-help {pair [] args} "op>=") => (s-app (s-id (s-global "_greaterequal")) args)
  | (curry-binop-help {pair [] args} "op<=") => (s-app (s-id (s-global "_lessequal")) args)
  | (curry-binop-help {pair [] args} "op==") => (s-app (s-id (s-global "equal-always")) args)
  | (curry-binop-help {pair [] args} "op=~") => (s-app (s-id (s-global "equal-now")) args)
  | (curry-binop-help {pair [] args} "op<=>") => (s-app (s-id (s-global "identical")) args)
  | (curry-binop-help {pair [] args} "op<>") =>
    (s-prim-app "not" [(s-app (s-id (s-global "equal-always")) args)] (flat-prim-app))
  | (curry-binop-help {pair params args} op) =>
    (mk-s-lam params (curry-binop-help {pair [] args} op))
end

sugar check-bool:
  | (check-bool e) => (s-prim-app "checkWrapBoolean" [e] (flat-prim-app))
end

sugar s-op:
  | (s-op op-loc "opor" left right) =>
    (s-if-else [(s-if-branch left (s-bool true))] (check-bool right) false)
  | (s-op op-loc "opand" left right) =>
    (s-if-else [(s-if-branch left (check-bool right))] (s-bool false) false)
  | (s-op op-loc "op^" left right) => (s-app right [left])
  | (s-op op-loc op left right) => (curry-binop [left right] op)
end

```

resugarer = RL.resugar(rules, {resugar: false, srclocExt: true})

# var current = nothing

# fun init-time():
#   current := time-now()
# end

# fun log-time(s):
#   lam(e) block:
#     now = time-now()
#     print("\n" + tostring(now - current) + ": " + s + "\n")
#     current := now
#     e
#   end
# end

fun resugar(ast, uri :: String):
  # _ = init-time()
  ast.visit(RV.ast-to-term-visitor)
    # ^ log-time("ast-to-term-visitor")
    ^ resugarer(_, uri)
    # ^ log-time("actual desugaring")
end
