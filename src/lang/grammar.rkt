#lang ragg

program: prelude block

end: "end" | ";"

prelude: (import-stmt|provide-stmt)*

import-stmt: "import" (import-name | import-string) "as" NAME
import-name: NAME
import-string: STRING
provide-stmt: "provide" stmt end | "provide" "*"

block: stmt*

stmt: let-expr | fun-expr | data-expr | when-expr
    | var-expr | assign-expr | binop-expr | check-expr
    | graph-expr

let-expr: binding "=" binop-expr
binding: NAME ["::" ann]

fun-expr: "fun" fun-header ":" doc-string block where-clause end
fun-header: ty-params NAME args return-ann
ty-params:
  ["<" list-ty-param* NAME ">"]
list-ty-param: NAME ","
args: (PARENSPACE|PARENNOSPACE) [list-arg-elt* binding] ")"
list-arg-elt: binding ","
return-ann: ["->" ann]
doc-string: ["doc:" STRING]
where-clause: ["where:" block]

check-expr: "check:" block end

data-expr: "data" NAME ty-params data-mixins ":" data-variant* data-sharing where-clause end
data-mixins: ["deriving" mixins]
data-variant: "|" NAME variant-members data-with | "|" NAME data-with
variant-members: (PARENSPACE|PARENNOSPACE) [list-variant-member* variant-member] ")"
list-variant-member: variant-member ","
variant-member: ["mutable"|"cyclic"] binding
data-with: ["with:" fields]
data-sharing: ["sharing:" fields]

mixins: list-mixin* binop-expr
list-mixin: binop-expr ","

var-expr: "var" binding "=" binop-expr
assign-expr: NAME ":=" binop-expr

graph-expr: "graph:" let-expr* end

when-expr: "when" binop-expr ":" block end

binop-expr: not-expr | binop-expr binop binop-expr | expr

not-expr: "not" expr

binop: "+"  | "-"  | "*"  | "/"  | "<="  | ">="  | "=="
     | "<>"  | "<"  | ">" | "and" | "or" | "is" | "raises"

expr: paren-expr | id-expr | prim-expr
    | lambda-expr | method-expr | app-expr | left-app-expr
    | obj-expr | list-expr
    | dot-expr | bracket-expr | colon-expr | colon-bracket-expr
    | get-bang-expr | update-expr
    | extend-expr
    | if-expr | cases-expr
    | for-expr | try-expr
    | user-block-expr

# paren-exprs must be preceded by a space, so as not be be confused with
# function application
paren-expr: PARENSPACE binop-expr ")"

id-expr: NAME

prim-expr: num-expr | bool-expr | string-expr

num-expr: NUMBER | "-" NUMBER
bool-expr: "true" | "false"
string-expr: STRING

lambda-expr: "fun" ty-params [args] return-ann ":" doc-string block where-clause end

method-expr: "method" args return-ann ":" doc-string block where-clause end

app-expr: expr app-args
# application must have the function expression immediately adjacent to
# the argument list, so as not to be confused with parenthesized exprs
app-args: PARENNOSPACE [app-arg-elt* binop-expr] ")"
app-arg-elt: binop-expr ","

left-app-expr: expr "^" left-app-fun-expr app-args
left-app-fun-expr: id-expr | id-expr "." NAME

obj-expr: "{" obj-fields "}" | "{" "}"
obj-fields: list-obj-field* obj-field [","]
list-obj-field: obj-field ","
obj-field: key ":" binop-expr
     | "mutable" key ["::" ann] ":" binop-expr
     | key args return-ann ":" doc-string block where-clause end

fields: list-field* field [","]
list-field: field ","
field: key ":" binop-expr
     | key args return-ann ":" doc-string block where-clause end
key: NAME | "[" binop-expr "]"

list-elt: binop-expr ","
list-expr: "[" [list-elt* binop-expr] "]"

dot-expr: expr "." NAME
bracket-expr: expr "." "[" binop-expr "]"

get-bang-expr: expr "!" NAME

colon-expr: expr ":" NAME
colon-bracket-expr: expr ":" "[" binop-expr "]"

extend-expr: expr "." "{" fields "}"
update-expr: expr "!" "{" fields "}"

if-expr: "if" binop-expr ":" block else-if* ["else:" block] end
else-if: "else if" binop-expr ":" block

cases-expr: "cases" (PARENSPACE|PARENNOSPACE) ann ")" expr ":" cases-branch* ["|" "else" "=>" block] end
cases-branch: "|" NAME [args] "=>" block

for-bind: binding "from" binop-expr
for-bind-elt: for-bind ","
for-expr: "for" expr PARENNOSPACE [for-bind-elt* for-bind] ")" return-ann ":" block end

try-expr: "try:" block "except" (PARENSPACE|PARENNOSPACE) binding ")" ":" block end

user-block-expr: "block:" block end

ann: name-ann | record-ann | arrow-ann | app-ann | pred-ann | dot-ann

name-ann: NAME
record-ann: "{" [list-ann-field* ann-field] "}"
          | "{" "}"
list-ann-field: ann-field ","
ann-field: NAME ":" ann

arrow-ann: (PARENSPACE|PARENNOSPACE) arrow-ann-elt* ann "->" ann ")"
arrow-ann-elt: ann ","

app-ann: (name-ann|dot-ann) "<" app-ann-elt* ann ">"
app-ann-elt: ann ","

pred-ann: ann (PARENSPACE|PARENNOSPACE) binop-expr ")"

dot-ann : NAME "." NAME
