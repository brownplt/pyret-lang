const R = require('requirejs');

R.config({
    paths: {
        'jglr': "../jglr",
        'nearley': ".",
    }
});

R(["nearley/nearley-tokenizer"], function (T) {
    const Tokenizer = T.Tokenizer;
    Tokenizer.reset("2 + 3");
    console.log(Tokenizer.next());
});