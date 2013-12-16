#lang ragg

program: prelude block

end: END | SEMI

prelude: [provide-stmt] import-stmt*

import-stmt: IMPORT (import-name | import-string) AS NAME
import-name: NAME
import-string: STRING
provide-stmt: PROVIDE stmt end | PROVIDE STAR

block: stmt*

stmt: let-expr | fun-expr | data-expr | datatype-expr | when-expr
    | var-expr | assign-expr | check-test | check-expr
    | graph-expr

let-expr: binding EQUALS binop-expr
binding: NAME [COLONCOLON ann]

fun-expr: FUN fun-header COLON doc-string block where-clause end
fun-header: ty-params NAME args return-ann
ty-params:
  [LT list-ty-param* NAME GT]
list-ty-param: NAME COMMA
args: (PARENSPACE|PARENNOSPACE) [list-arg-elt* binding] RPAREN
list-arg-elt: binding COMMA
return-ann: [THINARROW ann]
doc-string: [DOC STRING]
where-clause: [WHERE block]

check-expr: CHECK block end
check-test: binop-expr check-op binop-expr | binop-expr

data-expr: DATA NAME ty-params data-mixins COLON [first-data-variant] data-variant* data-sharing where-clause end
data-mixins: [DERIVING mixins]
first-data-variant: NAME variant-members data-with | NAME data-with
data-variant: BAR NAME variant-members data-with | BAR NAME data-with
variant-members: (PARENSPACE|PARENNOSPACE) [list-variant-member* variant-member] RPAREN
list-variant-member: variant-member COMMA
variant-member: [MUTABLE|CYCLIC] binding
data-with: [WITH fields]
data-sharing: [SHARING fields]

mixins: list-mixin* binop-expr
list-mixin: binop-expr COMMA

datatype-expr: DATATYPE NAME ty-params COLON [first-datatype-variant] datatype-variant* where-clause end
first-datatype-variant: NAME variant-members constructor-clause | NAME constructor-clause
datatype-variant: BAR NAME variant-members constructor-clause | BAR NAME constructor-clause
constructor-clause: WITHCONSTRUCTOR (PARENSPACE|PARENNOSPACE) NAME RPAREN COLON block end

var-expr: VAR binding EQUALS binop-expr
assign-expr: NAME COLONEQUALS binop-expr

graph-expr: GRAPH let-expr* end

when-expr: WHEN binop-expr COLON block end

binop-expr: binop-clause (binop binop-clause)*
binop-clause: not-expr | expr

not-expr: NOT expr

binop: PLUS | DASH | STAR | SLASH | LEQ | GEQ | EQUALEQUAL
     | NEQ  | LT  | GT | AND | OR

check-op: IS | RAISES | SATISFIES

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
paren-expr: PARENSPACE binop-expr RPAREN

id-expr: NAME

prim-expr: num-expr | bool-expr | string-expr

num-expr: NUMBER | DASH NUMBER
bool-expr: TRUE | FALSE
string-expr: STRING

lambda-expr: FUN ty-params [args] return-ann COLON doc-string block where-clause end

method-expr: METHOD args return-ann COLON doc-string block where-clause end

app-expr: expr app-args
# application must have the function expression immediately adjacent to
# the argument list, so as not to be confused with parenthesized exprs
app-args: PARENNOSPACE [app-arg-elt* binop-expr] RPAREN
app-arg-elt: binop-expr COMMA

left-app-expr: expr CARET left-app-fun-expr app-args
left-app-fun-expr: id-expr | id-expr DOT NAME

obj-expr: LBRACE obj-fields RBRACE | LBRACE RBRACE
obj-fields: list-obj-field* obj-field [COMMA]
list-obj-field: obj-field COMMA
obj-field: key COLON binop-expr
     | MUTABLE key [COLONCOLON ann] COLON binop-expr
     | key args return-ann COLON doc-string block where-clause end

fields: list-field* field [COMMA]
list-field: field COMMA
field: key COLON binop-expr
     | key args return-ann COLON doc-string block where-clause end
key: NAME | LBRACK binop-expr RBRACK

list-elt: binop-expr COMMA
list-expr: LBRACK [list-elt* binop-expr] RBRACK

dot-expr: expr DOT NAME
bracket-expr: expr DOT LBRACK binop-expr RBRACK

get-bang-expr: expr BANG NAME

colon-expr: expr COLON NAME
colon-bracket-expr: expr COLON LBRACK binop-expr RBRACK

extend-expr: expr DOT LBRACE fields RBRACE
update-expr: expr BANG LBRACE fields RBRACE

if-expr: IF binop-expr COLON block else-if* [ELSE block] end
else-if: ELSEIF binop-expr COLON block

cases-expr: CASES (PARENSPACE|PARENNOSPACE) ann RPAREN expr COLON cases-branch* [BAR ELSE THICKARROW block] end
cases-branch: BAR NAME [args] THICKARROW block

for-bind: binding FROM binop-expr
for-bind-elt: for-bind COMMA
for-expr: FOR expr PARENNOSPACE [for-bind-elt* for-bind] RPAREN return-ann COLON block end

try-expr: TRY  block EXCEPT (PARENSPACE|PARENNOSPACE) binding RPAREN COLON block end

user-block-expr: BLOCK block end

ann: name-ann | record-ann | arrow-ann | app-ann | pred-ann | dot-ann

name-ann: NAME
record-ann: LBRACE [list-ann-field* ann-field] RBRACE
          | LBRACE RBRACE
list-ann-field: ann-field COMMA
ann-field: NAME COLON ann | NAME COLONCOLON ann

arrow-ann: (PARENSPACE|PARENNOSPACE) [arrow-ann-elt* ann] THINARROW ann RPAREN
arrow-ann-elt: ann COMMA

app-ann: (name-ann|dot-ann) LT app-ann-elt* ann GT
app-ann-elt: ann COMMA

pred-ann: ann (PARENSPACE|PARENNOSPACE) binop-expr RPAREN

dot-ann : NAME DOT NAME
