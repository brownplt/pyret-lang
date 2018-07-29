({
    requires: [
        { "import-type": "builtin", name: "srcloc" },
        { "import-type": "builtin", name: "ast" },
        { "import-type": "builtin", name: "lists" }
    ],
    nativeRequires: [
        'resugar/resugar',
    ],
    provides: {},
    theModule: function(RUNTIME, NAMESPACE, uri, srclocLib, astLib, listsLib, resugarLib) {
        var get = RUNTIME.getField;
        var makeObject = RUNTIME.makeObject;

        var srcloc = get(srclocLib, "values");
        var ast = get(astLib, "values");
        var lists = get(listsLib, "values");

        var sName = get(ast, 's-name');
        var dummyLoc = get(ast, 'dummy-loc');

        var builtinConstruct = get(srcloc, 'builtin');
        var srclocConstruct = get(srcloc, 'srcloc');

        function deserialize(s, uri) {
            if (s[0] === 'b') {
                return builtinConstruct.app(s.substring(1));
            }
            var arr = s.split(',').map(Number);
            return srclocConstruct.app(uri, arr[0], arr[1], arr[2], arr[3], arr[4], arr[5]);
        }

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

        function encode(top, uri) {
            function loop(obj) {
                switch (obj.t) {
                case 'St': return obj.v;
                case 'N':
                    return RUNTIME.ffi.cases(
                        RUNTIME.ffi.isOption, "Option",
                        RUNTIME.string_to_number(obj.v), {
                            none: function () {
                                throw new Error(obj.v + " is not a number");
                            },
                            some: function (v) {
                                return v;
                            }
                        });
                case 'Lo': return deserialize(obj.v, uri);
                case 'B': return obj.v;
                case 'Var': return sName.app(dummyLoc, obj.v);
                case 'C':
                    var args = obj.ps.map(loop);
                    switch (obj.n) {
                    case 's-construct-normal':
                    case 's-construct-lazy':
                    case 'ASCENDING':
                    case 'DESCENDING':
                    case 's-normal':
                    case 's-mutable':
                    case 's-cases-bind-ref':
                    case 's-cases-bind-normal':
                    case 'a-blank':
                        return get(ast, obj.n);
                    }
                    switch (obj.n) {
                    case 's-global':
                    case 's-type-global':
                    case 's-atom':
                    case 'app-info-c':
                    case 'prim-app-info-c':
                    case 's-defined-value':
                    case 's-defined-var':
                    case 's-defined-type':
                    case 'a-checked':
                        args.shift();
                    }
                    var constr = get(ast, obj.n);
                    return constr.app.apply(constr, args);
                case 'None': return RUNTIME.ffi.makeNone();
                case 'Some': return RUNTIME.ffi.makeSome(loop(obj.v));
                case 'L': return RUNTIME.raw_array_to_list(obj.v.map(loop));
                default: throw new Error("Unexpected type: " + obj.t);
                }
            }
            return loop(top);
        }

        function resugar(rules, settings) {
            var runJSON = resugarLib.runJSON(rules, {
                resugar: RUNTIME.getField(settings, 'resugar'),
                srclocExt: RUNTIME.getField(settings, 'srclocExt')
            });
            return RUNTIME.makeFunction(function(term, uri) {
                return encode(runJSON(decode(term)), uri);
            }, 'resugar');
        }

        return RUNTIME.makeModuleReturn({
            resugar: RUNTIME.makeFunction(resugar, 'resugar')
        }, {});
    }
})
