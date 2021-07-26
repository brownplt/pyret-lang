const R = require("requirejs");

R.config({
    paths: {
        'jglr': "../../../jglr",
        'test-base': ".",
        'src-base': ".."
    }
});

R(["src-base/pyret-tokenizer", "test-base/parser"], function (T, G) {
    const tokenizer = T.Tokenizer;
    tokenizer.tokenizeFrom("2 + ");
    const parsed = G.TestGrammar.parse(tokenizer);
    if (parsed) {
        const countParses = G.TestGrammar.countAllParses(parsed);
        if (countParses === 1) {
            const ast = G.TestGrammar.constructUniqueParse(parsed);
            const astStr = ast.toString(true);
            console.log(astStr);
            console.log(JSON.stringify(ast, null, 2));
        }
        console.log("Parsed");
    } else {
        console.log("Failed to parse");
    }
});