const R = require("requirejs");

R.config({
    paths: {
        'jglr': "../../../jglr",
        'pyret-base': ".",
        'src-base': ".."
    }
});

R(["src-base/pyret-tokenizer", "pyret-base/parser", "fs"], function (T, G, fs) {
    const data = fs.readFileSync(process.argv[2], {encoding: "utf8"});
    const tokenizer = T.Tokenizer;
    tokenizer.tokenizeFrom(data);
    const parsed = G.PyretGrammar.parse(tokenizer);
    if (parsed) {
        const countParses = G.PyretGrammar.countAllParses(parsed);
        if (countParses === 1) {
            const ast = G.PyretGrammar.constructUniqueParse(parsed);
            const astStr = ast.toString(true);
            console.log(astStr);
            //console.log(JSON.stringify(ast, null, 2));
        }
    } else {
        console.log("Failed to parse");
    }
});
