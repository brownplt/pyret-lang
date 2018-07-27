({
    requires: [
        // { "import-type": "builtin", name: "srcloc" },
        // { "import-type": "builtin", name: "ast" },
        // { "import-type": "builtin", name: "lists" }
    ],
    nativeRequires: [
        "resugar/resugar",
    ],
    provides: {},
    theModule: function(RUNTIME, NAMESPACE, uri, /* srclocLib, astLib, listsLib, */ resugarLib) {
        // var srcloc = RUNTIME.getField(srclocLib, "values");
        // var ast = RUNTIME.getField(astLib, "values");
        // var lists = RUNTIME.getField(listsLib, "values");

        // var link = RUNTIME.getField(lists, "link");
        // var empty = RUNTIME.getField(lists, "empty");

        function resugar(rules) {
            // no resugar, yes srclocExt
            return RUNTIME.makeFunction(resugarLib.runJSON(rules, [false, true]), "resugar");
        }

        return RUNTIME.makeModuleReturn({
            'resugar': RUNTIME.makeFunction(resugar, "resugar")
        }, {});
    }
})
