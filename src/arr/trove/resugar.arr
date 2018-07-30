provide *

import global as _
import resugar-lib as RL
import resugar-visitor as RV
import json as JSON

rules = ```
sugar flat-prim-app:
  | (flat-prim-app) => (prim-app-info-c false)
end

sugar non-flat-prim-app:
  | (non-flat-prim-app) => (prim-app-info-c true)
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
  | (table-order-map <s-name column> {ASCENDING}) =>
    (s-array [(s-bool true) (s-str (biject name-to-str column))])
  | (table-order-map <s-name column> {DESCENDING}) =>
    (s-array [(s-bool false) (s-str (biject name-to-str column))])
end

################################################################################
# DONE: s-method-field
################################################################################

sugar s-method-field:
  | (s-method-field name params args ann doc body check-loc check blocky) =>
    (s-data-field name (s-method name params args ann doc body
                                 check-loc check blocky))
end

################################################################################
# DONE: s-table and s-load-table
################################################################################

sugar s-field-name:
  | (s-field-name @l name ann) => {s-field-name l name ann}
end

sugar s-table-row:
  | (s-table-row elems) => {s-table-row elems}
end

sugar s-table:
  | (s-table [{s-field-name l_{i} name_{i} ann_{i}} ...i]
             [{s-table-row [val_{i j} ...i]} ...j]) =>
    (s-prim-app "makeTable"
      [(s-array [(s-str @l_{i} name_{i}) ...i])
       (s-array [(s-array [(check-ann val_{i j} ann_{i}) ...i]) ...j])]
      (flat-prim-app))
end

################################################################################

sugar s-sanitize:
  | (s-sanitize name sanitizer) => {s-sanitize name sanitizer}
end

sugar s-table-src:
  | (s-table-src src) => {s-table-src src}
end

sugar load-table-acc:
  # if nothing is left in the to-do queue, returns
  | (load-table-acc [] src sanitizers) => {pair src sanitizers}
  # if we have a sanitizer, pop it from the to-do queue to the result
  | (load-table-acc [{s-sanitize <s-name name> sanitizer} rest_{i} ...i]
                    src
                    [sanitizers_{j} ...j]) =>
    (load-table-acc [rest_{i} ...i] src
       [(s-app (b-id "as-loader-option")
          [(s-str "sanitizer")
           (s-str (biject name-to-str name))
           sanitizer]) sanitizers_{j} ...j])
  # if we have a source, pop it from the to-do queue to the result.
  # Note that there should be no source in the result already
  | (load-table-acc [{s-table-src src} rest_{i} ...i] none sanitizers) =>
    (load-table-acc [rest_{i} ...i] {some src} sanitizers)
  # Otherwise, it means there's source already, so WF error
  | (load-table-acc _ {some _} _) =>
    {ERROR "well-formedness" "there are more one source"}
end

sugar s-load-table:
  | (s-load-table headers spec) => (load-table-helper headers (load-table-acc spec none []))
end

sugar load-table-helper:
  | (load-table-helper [{s-field-name l_{i} name_{i} ann_{i}} ...i]
                       {pair {some src} sanitizers}) =>
    (s-app (b-id "open-table")
       [(s-app (s-dot src "load") [(s-array [(s-str name_{i}) ...i])
                                   (s-array sanitizers)])])
  | (load-table-helper _ {pair none sanitizers}) =>
    {ERROR "well-formedness" "s-load-table missing source"}
end

################################################################################
# DONE: s-spy-block
################################################################################

sugar s-spy-expr:
  | (s-spy-expr @l name value _) => {s-spy-expr (s-srcloc l) (s-str name) value}
end

sugar s-spy-block-message:
  | (s-spy-block-message none) => (s-str "")
  | (s-spy-block-message {some msg}) => msg
end

sugar s-spy-block:
  | (s-spy-block @l message [{s-spy-expr loc_{i} name_{i} value_{i}} ...i]) =>
    (s-app (b-id "spy")
      [(s-srcloc l)
       (s-spy-block-message message)
       (s-array [loc_{i} ...i])
       (s-array [name_{i} ...i])
       (s-array [value_{i} ...i])])
end

################################################################################

sugar curry-args-acc:
  | (curry-args-acc [] [param_{x} ...x] args) =>
    {pair (biject reverse [(mk-s-bind param_{x}) ...x]) (biject reverse args)}
  | (curry-args-acc [<s-id <s-underscore>> rest_{x} ...x]
                    [param_{y} ...y]
                    [arg_{z} ...z]) =>
    (fresh f
      (curry-args-acc [rest_{x} ...x]
                      [(s-name f) param_{y} ...y]
                      [(s-id (s-name f)) arg_{z} ...z]))
  | (curry-args-acc [e rest_{x} ...x] [param_{y} ...y] [arg_{z} ...z]) =>
    (curry-args-acc [rest_{x} ...x] [param_{y} ...y] [e arg_{z} ...z])
end

sugar curry-args:
  | (curry-args lst) => (curry-args-acc lst [] [])
end

################################################################################

sugar curry-binop:
  | (curry-binop args op) => (curry-binop-help (curry-args args) op)
end

# @blener says that the current behavior of _ + _ + _ is wrong, but it's been wrong
# in the old desugaring too.

sugar curry-binop-help:
  | (curry-binop-help {pair [] args} "op+") => (s-app (g-id "_plus") args)
  | (curry-binop-help {pair [] args} "op-") => (s-app (g-id "_minus") args)
  | (curry-binop-help {pair [] args} "op*") => (s-app (g-id "_times") args)
  | (curry-binop-help {pair [] args} "op/") => (s-app (g-id "_divide") args)
  | (curry-binop-help {pair [] args} "op<") => (s-app (g-id "_lessthan") args)
  | (curry-binop-help {pair [] args} "op>") => (s-app (g-id "_greaterthan") args)
  | (curry-binop-help {pair [] args} "op>=") => (s-app (g-id "_greaterequal") args)
  | (curry-binop-help {pair [] args} "op<=") => (s-app (g-id "_lessequal") args)
  | (curry-binop-help {pair [] args} "op==") => (s-app (g-id "equal-always") args)
  | (curry-binop-help {pair [] args} "op=~") => (s-app (g-id "equal-now") args)
  | (curry-binop-help {pair [] args} "op<=>") => (s-app (g-id "identical") args)
  | (curry-binop-help {pair [] args} "op<>") =>
    (s-prim-app "not" [(s-app (g-id "equal-always") args)] (flat-prim-app))
  | (curry-binop-help @l {pair [] [e1 e2]} "op[]") =>
    (s-prim-app "getBracket" [(s-srcloc l) e1 e2] (non-flat-prim-app))
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

################################################################################

sugar s-app-help:
  | (s-app-help {pair [] [f args_{x} ...x]}) => <s-app f [args_{x} ...x]>
  | (s-app-help {pair params args}) => (mk-s-lam params (s-app-help {pair [] args}))
end

sugar s-app:
  | (s-app f [args_{x} ...x]) => (s-app-help (curry-args [f args_{x} ...x]))
end

sugar s-method-app-help:
  | (s-method-app-help {pair [] [obj args_{i} ...i]} l-dot field) =>
    <s-app <s-dot @l-dot obj field> [args_{i} ...i]>
  | (s-method-app-help {pair params args} l-dot field) =>
    (mk-s-lam params (s-method-app-help {pair [] args} l-dot field))
end

sugar s-method-app:
  | (s-method-app l-dot obj field [args_{i} ...i]) =>
    (s-method-app-help (curry-args [obj args_{i} ...i]) l-dot field)
end

sugar s-dot:
  | (s-dot <s-id <s-underscore>> field) =>
    (fresh x (mk-s-lam [(mk-s-bind (s-name x))] <s-dot (s-id (s-name x)) field>))
  | (s-dot x field) => <s-dot x field>
end

################################################################################

sugar s-bracket:
  | (s-bracket obj key) => (curry-binop [obj key] "op[]")
end

sugar s-get-bang:
  | (s-get-bang <s-id <s-underscore>> field) =>
    (fresh x (mk-s-lam [(mk-s-bind (s-name x))] <s-get-bang (s-id (s-name x)) field>))
  | (s-get-bang x field) => <s-get-bang x field>
end

sugar s-update:
  | (s-update <s-id <s-underscore>> fields) =>
    (fresh x (mk-s-lam [(mk-s-bind (s-name x))] <s-update (s-id (s-name x)) fields>))
  | (s-update x field) => <s-update x field>
end

sugar s-extend:
  | (s-extend <s-id <s-underscore>> fields) =>
    (fresh x (mk-s-lam [(mk-s-bind (s-name x))] <s-extend (s-id (s-name x)) fields>))
  | (s-extend x field) => <s-extend x field>
end

###############################################################################

sugar transform-underscore:
  | (transform-underscore <s-underscore>) => (fresh name (s-name name))
  | (transform-underscore non-underscore) => non-underscore
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

fun resugar(ast, uri :: String) block:
  # _ = init-time()
  ast.visit(RV.ast-to-term-visitor)
    # ^ log-time("ast-to-term-visitor")
    ^ resugarer(_, uri)
    # ^ log-time("actual desugaring")
end
