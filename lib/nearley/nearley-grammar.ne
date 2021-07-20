@{%
const R = require('requirejs');

R.config({
    paths: {
        'jglr': "../jglr",
        'nearley': ".",
    }
});

const T = R("nearley/nearley-tokenizer");
const tokenizer = T.Tokenizer;
%}

@lexer tokenizer 

P -> S
S -> S %PLUS M | M
M -> M %TIMES T | T
T -> %NUMBER