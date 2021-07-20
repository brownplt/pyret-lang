const R = require('requirejs');

R.config({
    paths: {
        'jglr': "../jglr",
        'nearley': ".",
    }
});

const T = R("nearley/nearley-tokenizer");
const fs = R("fs");

const data = fs.readFileSync(process.argv[2], {encoding: "utf8"});

const t = T.Tokenizer;
console.log("Tokenize from: \n" + data + "\n");
t.reset(data);

console.log("Tokens: \n");
while (t.hasNext()) {
    const tok = t.next();
    console.log(JSON.stringify(tok, null, 2));
}