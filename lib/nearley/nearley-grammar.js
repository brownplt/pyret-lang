// Generated automatically by nearley, version 2.20.1
// http://github.com/Hardmath123/nearley
(function () {
function id(x) { return x[0]; }

const R = require('requirejs');

R.config({
    paths: {
        'jglr': "../jglr",
        'nearley': ".",
    }
});

const T = R("nearley/nearley-tokenizer");
const tokenizer = T.Tokenizer;
var grammar = {
    Lexer: tokenizer,
    ParserRules: [
    {"name": "P", "symbols": ["S"]},
    {"name": "S", "symbols": ["S", (tokenizer.has("PLUS") ? {type: "PLUS"} : PLUS), "M"]},
    {"name": "S", "symbols": ["M"]},
    {"name": "M", "symbols": ["M", (tokenizer.has("TIMES") ? {type: "TIMES"} : TIMES), "T"]},
    {"name": "M", "symbols": ["T"]},
    {"name": "T", "symbols": [(tokenizer.has("NUMBER") ? {type: "NUMBER"} : NUMBER)]}
]
  , ParserStart: "P"
}
if (typeof module !== 'undefined'&& typeof module.exports !== 'undefined') {
   module.exports = grammar;
} else {
   window.grammar = grammar;
}
})();
