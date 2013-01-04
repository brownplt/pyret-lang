#lang ragg


## The following comes from: http://en.wikipedia.org/wiki/Backus%E2%80%93Naur_Form

<syntax> : <rule> | <rule> <syntax>
<rule>   : <opt-whitespace> "<" <RULE-NAME> ">" <opt-whitespace> "::=" 
                <opt-whitespace> <expression> <line-end>
<opt-whitespace> : " " <opt-whitespace> | ""  ## "" is empty string, i.e. no whitespace
<expression>     : <list> | <list> "|" <expression>
<line-end>       : <opt-whitespace> <EOL> | <line-end> <line-end>
<list>    : <term> | <term> <opt-whitespace> <list>
<term>    : <literal> | "<" <RULE-NAME> ">"
<literal> : '"' <TEXT> '"' | "'" <TEXT> "'"   ## actually, the original BNF did not use quotes
