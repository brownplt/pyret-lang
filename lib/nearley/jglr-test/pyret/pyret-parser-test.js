const R = require("requirejs");

R.config({
    paths: {
        'jglr': "../../../jglr",
        'pyret-base': ".",
        'src-base': ".."
    }
});

R(["src-base/pyret-tokenizer", "pyret-base/parser"], function (T, G) {
    const tokenizer = T.Tokenizer;
    tokenizer.tokenizeFrom("2 + 3");
    const parsed = G.PyretGrammar.parse(tokenizer);
    if (parsed) {
        const countParses = G.PyretGrammar.countAllParses(parsed);
        if (countParses === 1) {
            const ast = G.PyretGrammar.constructUniqueParse(parsed);
            const astStr = ast.toString(true);
            console.log(astStr);
            console.log(JSON.stringify(ast, null, 2));
        }
        console.log("Parsed");
    } else {
        console.log("Failed to parse");
    }
});
