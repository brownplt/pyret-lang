#lang ragg

## Statlist grammar

statlist : stat+
stat: ID '=' expr
    | 'print' expr 
    
expr: multExpr ('+' multExpr)*
multExpr: primary (('*'|'.') primary)*
primary : 
         INT
       | ID
       | '[' expr ("," expr)* ']'
