#lang ragg

program: imports block ENDMARKER

imports: (import-stmt|provide-stmt)*

import-name: NAME
import-string: STRING
import-stmt: "import" (import-name | import-string) "as" NAME
provide-stmt: "provide" stmt "end"

block: stmt*

stmt: (var-expr | let-expr | fun-expr | data-expr | do-expr | expr
    | assign-expr | when-expr | try-expr) [ENDMARKER]

expr: obj-expr | list-expr | app-expr | id-expr | prim-expr
    | dot-expr | bracket-expr | dot-method-expr | bracket-method-expr
    | cond-expr | lambda-expr | extend-expr | left-app-expr
    | for-expr

id-expr: NAME

assign-expr: NAME ":=" expr

prim-expr:
   num-expr
 | bool-expr
 | string-expr
num-expr: NUMBER | "-" NUMBER
bool-expr: "true" | "false"
string-expr: STRING
                    
var-expr: "var" arg-elt "=" expr
let-expr: arg-elt "=" expr

app-arg-elt: expr ","
app-args: "(" [app-arg-elt* expr] ")"
app-expr: expr app-args

arg-elt: NAME ["::" ann]
list-arg-elt: arg-elt ","
args: "(" [list-arg-elt* arg-elt] ")"

fun-body: block "end"
        | "(" block ")"

list-ty-param: NAME ","
ty-params:
  ["<" list-ty-param* NAME ">"]

return-ann: ["->" ann]

fun-header: ty-params NAME args return-ann

fun-expr: "fun" fun-header ":" fun-body
 
lambda-args: list-arg-elt* arg-elt
lambda-expr:
   BACKSLASH ty-params lambda-args return-ann ":" fun-body
 | BACKSLASH fun-body
 | BACKSLASH return-ann ":" fun-body
 
when-expr: "when" expr ":" block "end"

cond-branch: "|" expr "=>" block
cond-expr: "cond" ":" cond-branch* "end"

try-expr: "try" ":" block "except" "(" arg-elt ")" ":" block "end"
   
field:
   NAME ":" expr
 | NAME args return-ann ":" block "end"
 | "[" expr "]" ":" expr
 | "[" expr "]" args return-ann ":" block "end"
list-field: field ","
fields: list-field* field [","]

# list-field is here because it works better with syntax-matching -
# there's a syntax sub-list for list-field that we can grab hold of
obj-expr:
   "{" fields "}"
 | "{" "}"

list-elt: expr ","
list-expr: "[" [list-elt* expr] "]"

extend-expr: expr "." "{" fields "}"
             # if we want it, we can add | expr "." "{" expr "}"

dot-expr: expr "." NAME
bracket-expr: expr "." "[" expr "]"

left-app-fun-expr: id-expr | id-expr "." NAME
left-app-expr: expr "^" left-app-fun-expr app-args

dot-method-expr: expr ":" NAME
bracket-method-expr: expr ":" "[" expr "]"

data-with: ["with" fields]
data-variant: "|" NAME args data-with | "|" NAME data-with
data-sharing: "end"|("sharing" fields "end")
data-expr: "data" NAME ty-params ":" data-variant+ data-sharing 

do-stmt: block ";"
do-expr: "do" stmt do-stmt* block "end"

for-bind: arg-elt "from" expr
for-bind-elt: for-bind ","
for-expr: "for" expr "(" [for-bind-elt* for-bind] ")" return-ann ":" block "end"
           
ann: name-ann | record-ann | arrow-ann | app-ann | pred-ann | dot-ann

name-ann: NAME
record-ann: "{" [list-ann-field* ann-field] "}"
          | "{" "}"
ann-field: NAME ":" ann
list-ann-field: ann-field ","

arrow-ann-elt: ann ","
arrow-ann: "(" arrow-ann-elt* ann "->" ann ")"

app-ann-elt: ann ","
app-ann: name-ann "<" app-ann-elt* ann ">"

pred-ann: ann "(" expr ")"

dot-ann : NAME "." NAME
