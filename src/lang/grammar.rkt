#lang ragg

program: block ENDMARKER

block: stmt*

stmt: def-expr | fun-expr | data-expr | do-expr | expr
    | assign-expr | dot-assign-expr | bracket-assign-expr

expr: obj-expr | list-expr | app-expr | id-expr | prim-expr
    | dot-expr | bracket-expr | dot-method-expr | bracket-method-expr
    | cond-expr | lambda-expr

id-expr: NAME

assign-expr: NAME "=" expr

prim-expr:
   num-expr
 | bool-expr
 | string-expr
num-expr: NUMBER | "-" NUMBER
bool-expr: "true" | "false"
string-expr: STRING
                    
def-expr: "def" NAME ["::" ann] ":" expr

app-arg-elt: expr ","
app-args: "(" [app-arg-elt* expr] ")"
app-expr: expr app-args

arg-elt: NAME ["::" ann] ","
last-arg-elt: NAME ["::" ann]
args: "(" [arg-elt* last-arg-elt] ")"

fun-body: block "end"
        | "(" block ")"

fun-ty-param-elt: NAME
fun-ty-param: fun-ty-param-elt ","
fun-ty-params:
  "(" fun-ty-param* fun-ty-param-elt ")"

fun-header: [fun-ty-params] NAME args ["->" ann]

fun-expr: "fun" fun-header ":" fun-body
 
lambda-args: arg-elt* last-arg-elt
lambda-expr:
   BACKSLASH lambda-args ":" "(" block ")"
 | BACKSLASH lambda-args "->" ann ":" "(" block ")"
 | BACKSLASH "(" block ")"
 | BACKSLASH "->" ann ":" "(" block ")"
 
cond-branch: "|" expr "=>" block
cond-expr: "cond" ":" cond-branch* "end"
   
field:
   NAME ":" expr
 | NAME args ":" block ["end"]
list-field: field ","
fields: list-field* field [","]

# list-field is here because it works better with syntax-matching -
# there's a syntax sub-list for list-field that we can grab hold of
obj-expr:
   "{" ["extend" expr "with"] fields "}"
 | "{" "extend" expr "}"
 | "{" "}"

list-elt: expr ","
list-expr: "[" [list-elt* expr] "]"

dot-expr: expr "." NAME
bracket-expr: expr "." "[" expr "]"

dot-assign-expr: expr "." NAME = expr
bracket-assign-expr: expr "." "[" expr "]" "=" expr

dot-method-expr: expr ":" NAME
bracket-method-expr: expr ":" "[" expr "]"

data-member: NAME ["::" ann]
data-member-elt: data-member ","
data-variant:
   "|" NAME [":" data-member-elt* data-member] ["with" fields]
data-param-elt: NAME ","
data-params: "(" data-param-elt* NAME ")"
data-expr: "data" NAME [data-params] data-variant+ ("end"|("sharing" fields "end"))

do-stmt: block ";"
do-expr: "do" stmt do-stmt* block "end"
           
ann: name-ann | record-ann | arrow-ann | app-ann

name-ann: NAME
record-ann: "{" [list-ann-field* ann-field] "}"
ann-field: NAME ":" ann
list-ann-field: ann-field ","

arrow-ann-elt: ann ","
arrow-ann: "(" arrow-ann-elt* ann "->" ann ")"
app-ann-elt: ann ","
app-ann: name-ann "(" app-ann-elt* ann ")"
