#lang ragg

program: imports block

imports: (import-stmt|provide-stmt)*

import-name: NAME
import-string: STRING
import-stmt: "import" (import-name | import-string) "as" NAME
provide-stmt: "provide" stmt "end"

block: stmt*

stmt: (var-expr | let-expr | fun-expr | data-expr | do-expr | binop-expr
    | assign-expr | when-expr | try-expr)

binop: "+"  | "-"  | "*"  | "/"  | "<="  | ">="  | "==" | "<>"  | "<"  | ">"
    
binop-expr: expr | binop-expr binop binop-expr

# paren-exprs must be preceded by a space, so as not be be confused with
# function application
paren-expr: PARENSPACE binop-expr ")"
    
expr: obj-expr | list-expr | app-expr | id-expr | prim-expr
    | dot-expr | bracket-expr | dot-method-expr | bracket-method-expr
    | cond-expr | lambda-expr | method-expr | extend-expr | left-app-expr
    | for-expr | paren-expr


id-expr: NAME

assign-expr: NAME ":=" binop-expr

prim-expr:
   num-expr
 | bool-expr
 | string-expr
num-expr: NUMBER | "-" NUMBER
bool-expr: "true" | "false"
string-expr: STRING
                    
var-expr: "var" arg-elt "=" binop-expr
let-expr: arg-elt "=" binop-expr

app-arg-elt: binop-expr ","
app-args: PARENNOSPACE [app-arg-elt* binop-expr] ")"
app-expr: expr app-args

arg-elt: NAME ["::" ann]
list-arg-elt: arg-elt ","
args: PARENNOSPACE [list-arg-elt* arg-elt] ")"

fun-body: block "end"

list-ty-param: NAME ","
ty-params:
  ["<" list-ty-param* NAME ">"]

return-ann: ["->" ann]

fun-header: ty-params NAME args return-ann

fun-expr: "fun" fun-header ":" fun-body

lambda-expr: "fun" ty-params [args] return-ann ":" fun-body

method-expr: "method" args return-ann ":" fun-body

when-expr: "when" binop-expr ":" block "end"

cond-branch: "|" binop-expr "=>" block
cond-expr: "cond" ":" cond-branch* "end"

try-expr: "try" ":" block "except" (PARENSPACE|PARENNOSPACE) arg-elt ")" ":" block "end"
   
field:
   NAME ":" binop-expr
 | NAME args return-ann ":" block "end"
 | "[" binop-expr "]" ":" binop-expr
 | "[" binop-expr "]" args return-ann ":" block "end"
list-field: field ","
fields: list-field* field [","]

# list-field is here because it works better with syntax-matching -
# there's a syntax sub-list for list-field that we can grab hold of
obj-expr:
   "{" fields "}"
 | "{" "}"

list-elt: binop-expr ","
list-expr: "[" [list-elt* binop-expr] "]"

extend-expr: expr "." "{" fields "}"
             # if we want it, we can add | expr "." "{" expr "}"

dot-expr: expr "." NAME
bracket-expr: expr "." "[" binop-expr "]"

left-app-fun-expr: id-expr | id-expr "." NAME
left-app-expr: expr "^" left-app-fun-expr app-args

dot-method-expr: expr ":" NAME
bracket-method-expr: expr ":" "[" binop-expr "]"

data-with: ["with" fields]
data-variant: "|" NAME args data-with | "|" NAME data-with
data-sharing: "end"|("sharing" fields "end")
data-expr: "data" NAME ty-params ":" data-variant+ data-sharing 

do-stmt: block ";"
do-expr: "do" stmt do-stmt* block "end"

for-bind: arg-elt "from" binop-expr
for-bind-elt: for-bind ","
for-expr: "for" expr PARENNOSPACE [for-bind-elt* for-bind] ")" return-ann ":" block "end"
           
ann: name-ann | record-ann | arrow-ann | app-ann | pred-ann | dot-ann

name-ann: NAME
record-ann: "{" [list-ann-field* ann-field] "}"
          | "{" "}"
ann-field: NAME ":" ann
list-ann-field: ann-field ","

arrow-ann-elt: ann ","
arrow-ann: (PARENSPACE|PARENNOSPACE) arrow-ann-elt* ann "->" ann ")"

app-ann-elt: ann ","
app-ann: name-ann "<" app-ann-elt* ann ">"

pred-ann: ann (PARENSPACE|PARENNOSPACE) binop-expr ")"

dot-ann : NAME "." NAME
