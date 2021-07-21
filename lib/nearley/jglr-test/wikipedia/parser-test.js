const R = require("requirejs");

R.config({
    paths: {
        'jglr': "../../../jglr",
        'wiki-base': ".",
        'src-base': ".."
    }
});

R(["src-base/pyret-tokenizer", "wiki-base/parser"], function (T, G) {
    const tokenizer = T.Tokenizer;
    tokenizer.tokenizeFrom("2 + 3");
    const parsed = G.WikipediaGrammar.parse(tokenizer);
    if (parsed) {
        const countParses = G.WikipediaGrammar.countAllParses(parsed);
        if (countParses === 1) {
            const ast = G.WikipediaGrammar.constructUniqueParse(parsed);
            const astStr = ast.toString(true);
            console.log(astStr);
            console.log(JSON.stringify(ast, null, 2));
        }
        console.log("Parsed");
    } else {
        console.log("Failed to parse");
    }
});