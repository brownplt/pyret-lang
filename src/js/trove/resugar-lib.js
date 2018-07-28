({
    requires: [
        // { "import-type": "builtin", name: "srcloc" },
        // { "import-type": "builtin", name: "ast" },
        // { "import-type": "builtin", name: "lists" }
    ],
    nativeRequires: [
        'resugar/resugar',
    ],
    provides: {},
    theModule: function(RUNTIME, NAMESPACE, uri, /* srclocLib, astLib, listsLib, */ resugarLib) {
        // var srcloc = RUNTIME.getField(srclocLib, "values");
        // var ast = RUNTIME.getField(astLib, "values");
        // var lists = RUNTIME.getField(listsLib, "values");

        // var link = RUNTIME.getField(lists, "link");
        // var empty = RUNTIME.getField(lists, "empty");

        var get = RUNTIME.getField;
        var makeObject = RUNTIME.makeObject;

        function decode(obj) {
            switch (get(obj, 't')) {
            case 'St':
            case 'N':
            case 'Lo':
            case 'B':
            case 'Var':
                return {t: get(obj, 't'), v: get(obj, 'v')};
            case 'S':
                return {
                    t: 'S',
                    n: get(obj, 'n'),
                    ps: get(obj, 'ps').map(decode)
                };
            case 'None':
                return {t: 'None'};
            case 'Some':
                return {t: 'Some', v: decode(get(obj, 'v'))};
            case 'L':
                return {t: 'L', v: get(obj, 'v').map(decode)};
            default: throw new Error("Unexpected type: " + get(obj, 't'));
            }
        }

        function encode(obj) {
            switch (obj.t) {
            case 'St':
            case 'N':
            case 'Lo':
            case 'B':
            case 'Var': return makeObject(obj);
            case 'C': return makeObject({
                t: 'C',
                n: obj.n,
                ps: obj.ps.map(encode)
            });
            case 'None': return makeObject({
                t: 'None'
            });
            case 'Some': return makeObject({
                t: 'Some',
                v: encode(obj.v)
            });
            case 'L': return makeObject({
                t: 'L',
                v: obj.v.map(encode)
            });
            default: throw new Error("Unexpected type: " + obj.t);
            }
        }

        function resugar(rules, settings) {
            var runJSON = resugarLib.runJSON(rules, {
                resugar: RUNTIME.getField(settings, 'resugar'),
                srclocExt: RUNTIME.getField(settings, 'srclocExt')
            });
            return RUNTIME.makeFunction(function(term) {
                return encode(JSON.parse(runJSON(JSON.stringify(decode(term)))));
            }, 'resugar');
        }

        return RUNTIME.makeModuleReturn({
            resugar: RUNTIME.makeFunction(resugar, 'resugar')
        }, {});
    }
})
